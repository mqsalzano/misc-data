
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%
%Section 1:
%Pull perception/mechanical data into MatLab and create a table.

clear all; clc;

%grab Excel file
[file,path] = uigetfile('*.xlsx');

%Write Excel sheet into a table
xl_table = readtable(strcat(path,file));

%Grab unique shoes from Excel file while keeping pairing with season, and
%index their location so Excel sheet can be subsetted using the unique shoe
%list
%shoe, etc.
TableVars = fieldnames(xl_table);
[idx, tf] = listdlg('ListString', TableVars);
ModXL = xl_table(:, TableVars(idx));

ShoeName = strings(length(xl_table.FirstName),1);
TestShoeVersion = strings(length(xl_table.FirstName),1);
SubjInitials = strings(length(xl_table.FirstName),1);
TestShoeGen = strings(length(xl_table.FirstName),1);
TestSeasonYr = strings(length(xl_table.FirstName),1);

for num = 1:length(xl_table.FirstName)
    
    tmpFname = char(ModXL.FirstName(num));
    tmpLname = char(ModXL.LastName(num));
    
    tmpFinitial = tmpFname(1);
    tmpLinitial = tmpLname(1);
    
    SubjInitials(num) = strcat(tmpFinitial,tmpLinitial);
    ShoeName(num) = char(ModXL.FullShoeName(num));
    TestShoeVersion(num) = char(ModXL.TestShoeSPAM(num));
    TestShoeGen(num) = strcat(char(ModXL.TestShoe(num)), " ", num2str(ModXL.TestShoeGeneration(num)));
    TestSeasonYr(num) = strcat(char(ModXL.TestSeason(num)), num2str(ModXL.TestSeason_Year(num)));
end

ShoeParameters = table(ShoeName, SubjInitials, TestShoeGen, TestShoeVersion, TestSeasonYr);
newXL = [ShoeParameters xl_table];

[ExcelShoeListUnq, xl_f2u, xl_u2f] = unique(ShoeParameters(:,[1 3]), 'stable');

UnqShoeParameters = ModXL(xl_f2u, :);

clearvars -except newXL ExcelShoeListUnq ShoeParameters UnqShoeParameters xl_table xl_f2u xl_u2f ModXL

save('BigCushExcel.mat')

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%Section 2

ShoeListSeason = struct;
InitialsListSeason = struct;
TestShoeListSeason = struct;
TestShoeVListSeason = struct;

%MATfiledata = struct;
%tmpvarlist = {};

fixedshoes = struct;
weirdshoes = struct;

for season_num = 1%:4
    datafolder = uigetdir('Select folder with files of interest');
    subjectFiles = dir(datafolder);
    subjectFiles(ismember({subjectFiles.name},{'.','..'})) = [];
    
    %get season from data folder
    slash_idx = strfind(datafolder, '/');
    season = datafolder(slash_idx(end)+1:end);
    
    
    ShoeListSeason.(season) = {};
    InitialsListSeason.(season) = {};
    TestShoeListSeason.(season) = {};
    TestShoeVListSeason.(season) = {};
    
    for file_num = 1:length(subjectFiles)
        
        %count = count + 1
        
        MATfilename = strcat(datafolder, '/', subjectFiles(file_num).name);
        load(MATfilename)
        
        %create a blank string array to fill that is length of the number of files
        %in FILE_NAME, with the exception of the sock conditions
        
        filepathlist = strings(length(FILE_NAME),1);
        
        %FILE_NAME is a cell array, which is hard to index
        %loop through FILE_NAME to turn each line (AKA file name) into a string
        for i = 1:(length(FILE_NAME))
            filepathlist(i) = char(FILE_NAME(i));
        end
        
        [filelist, testshoe, TS_version] = grabfilename(filepathlist);
        
        
        
        [shoes, initials] = getShoeNames(filelist);
        
        %put shoe names into a list
        [tmpshoes, ia, ic] = unique(shoes, 'stable');
        tmpInitials = initials(ia);
        tmpTS = testshoe(ia);
        tmpTSv = TS_version(ia);
        
        %create a list of the variables in workspace
        VarList = who;
        if isempty(tmpvarlist) == 1
            [indx, tf] = listdlg('ListString', VarList);
            tmpvarlist = VarList(indx);
        else
        end
        
        %convert variable names to string and the use eval() to get values
        %embedded in that variable
        
        for varnum = 1:length(tmpvarlist)
            tmpvarstring = string(tmpvarlist(varnum));
            tmpvar(:,varnum) = eval(tmpvarstring);
        end
        
        %According to naming convention, dynamic trials are always odd-numbered
        %while static trials are even-numbered. All variables but Leg Length
        %are taken from the dynamic trials but Leg Length is from the static.
        
        %VarVal contains data from both dynamic and static, but we need to keep
        %the the correct data for each variable.  "ia" is the index of the
        %dynamic trials (i.e. indx of 1, 3, 5,....).  Here, the code loops through the chosen variable list
        %and if Leg Length is one of them, then it will choose the
        %even-numbered data (i.e. ia+1)
        
        for x = 1:length(tmpvarlist)
            if strcmp(char(tmpvarlist(x)), 'Leg_Length') == 1
                VarVal(:,x) = tmpvar(ia+1,x);
            else
                VarVal(:,x) = tmpvar(ia,x);
            end
        end
        
        emptycell = cellfun('isempty', VarVal);
        VarVal(emptycell) = {NaN};
        
        ShoeListSeason.(season) = [ShoeListSeason.(season); tmpshoes];
        InitialsListSeason.(season) = [InitialsListSeason.(season); tmpInitials];
        TestShoeListSeason.(season) = [TestShoeListSeason.(season); tmpTS];
        TestShoeVListSeason.(season) = [TestShoeVListSeason.(season); tmpTSv];
        tmpseason(1:length(tmpshoes),1) = cellstr(season);
         
        if exist('tmpvarlist', 'var') == 1
            ColNames = ['Shoes','Initials','TestShoe', 'Version', 'Season', tmpvarlist'];
            tempdata = [tmpshoes, tmpInitials, tmpTS, tmpTSv, tmpseason, VarVal];
        else
            ColNames = {'Shoes','Initials','TestShoe', 'Version', 'Season'};
            tempdata = [tmpshoes, tmpInitials, tmpTS, tmpTSv, tmpseason];
        end
        
        if ismember(season, fieldnames(MATfiledata)) == 1
            MATfiledata.(season) = [MATfiledata.(season); cellstr(tempdata)];
        else
            MATfiledata.(season) = array2table(tempdata, 'VariableNames', ColNames);
        end
        
        clearvars -except ExcelShoeListUnq ShoeParameters UnqShoeParameters...
            xl_table TableVars season ShoeListSeason InitialsListSeason ...
            TestShoeListSeason TestShoeVListSeason datafolder subjectFiles...
            MATfiledata xl_f2u xl_u2f fixedshoes weirdshoes tmpvarlist
        
        %this is the end of the for-loop that goes through the folder
    end
    
    
    
    [SeasonShoeUnq, S_f2u, S_u2f] = unique(MATfiledata.(season).Shoes, 'stable');
    SeasonShoeUnq = MATfiledata.(season)(S_f2u, :);
    UnqXLshoe = unique(ExcelShoeListUnq.ShoeName);
    
        
    MATfiledata.(season) = fixshoename(...
        MATfiledata.(season), UnqXLshoe, SeasonShoeUnq, 1);
    
%     for shoe_num = 1:length(SeasonShoeUnq.Shoes) 
%         if ismember(SeasonShoeUnq.Shoes(shoe_num),UnqXLshoe) == 0
% 
%             x_idx = MATfiledata.(season).Shoes == SeasonShoeUnq.Shoes(shoe_num);
%             disp(SeasonShoeUnq(shoe_num,:))
%             [s_idx, tf] = listdlg('ListString', UnqXLshoe);
% 
%             if tf == 0
%                 tmpshoe = input('Type Shoe Name: ', 's');
%                 MATfiledata.(season).Shoes(x_idx) = tmpshoe;
%                
%             else
%                 MATfiledata.(season).Shoes(x_idx) = UnqXLshoe(s_idx);
%            
%             end 
%         else
%         end
%     end
    save('ShoeStructure.mat', 'MATfiledata', 'tmpvarlist')
    
    [SeasonTestShoeUnq, TS_f2u, TS_u2f] = unique(...
        MATfiledata.(season).TestShoe, 'stable');
    SeasonTestShoeUnq = MATfiledata.(season)(TS_f2u, :);
    UnqXLtestshoe = unique(ExcelShoeListUnq.TestShoeGen);

    MATfiledata.(season) = fixshoename(...
        MATfiledata.(season), UnqXLtestshoe, SeasonTestShoeUnq, 3);
    
%     for shoe_num = 1:length(SeasonTestShoeUnq.TestShoe)
%         if ismember(SeasonTestShoeUnq.TestShoe(shoe_num),UnqXLshoe) == 0
%             
%             x_idx = MATfiledata.(season).Shoes == SeasonShoeUnq.Shoes(shoe_num);
%             disp(SeasonTestShoeUnq(shoe_num,:))
%             [s_idx, tf] = listdlg('ListString', UnqXLtestshoe);
%             
%             if tf == 0
%                 tmptestshoe = input('Type Shoe Name: ', 's');
%                 MATfiledata.(season).TestShoe(x_idx) = tmptestshoe;
%                 
%             else
%                 MATfiledata.(season).TestShoe(x_idx) = UnqXLtestshoe(s_idx);
%                 
%             end
%         else
%         end
%     end
    
end %this is the end of the for-loop that goes through each season of testing



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%Section 3
%create a combined table from all test seasons

seasons = fieldnames(MATfiledata);

combinedTable = [];

for i = 1:length(seasons)
    
    season = char(seasons(i));
    tmptable = MATfiledata.(season);
    combinedTable = [combinedTable; tmptable];
    
end

combinedTable = sortrows(combinedTable);
ShoeParameters = sortrows(ShoeParameters);
newXL = sortrows(newXL);

combinedTable.Properties.VariableNames(3) = "StudyShoe"
combinedTable.Properties.VariableNames(135) = "BodyMass";

%[EqualTable, iCT, iSP] = intersect(table2array(combinedTable(:, 1:5)),...
   % table2array(ShoeParameters(:, 1:5)), 'rows');

[EqualTable2, iCT2, iSP2] = intersect(table2array(combinedTable(:, 1:5)),...
    table2array(newXL(:, 1:5)), 'rows');

%FinalTable = horzcat(combinedTable(iCT,:), ShoeParameters(iSP,:));
FinalTable2 = horzcat(combinedTable(iCT2,:), newXL(iSP2,:));
