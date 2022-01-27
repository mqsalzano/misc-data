function [VarVal, vars] = grabVariables(vars, VarList)
%UNTITLED Summary of this function goes here
%   Detailed explanation goes here
        if isempty(vars) == 0
            [indx, tf] = listdlg('ListString', VarList);
            tmpvarlist = VarList(indx);
        else
        end
        
       
        %convert variable names to string and the use eval() to get values
        %embedded in that variable
        if exist('tmpvarlist', 'var') == 1
            for varnum = 1:length(tmpvarlist)
                vars.(char(tmpvarlist(varnum))) = eval(char(tmpvarlist(varnum)));
            %tmpvar(:,varnum) = eval(tmpvarstring);
            end
            structfields = fieldnames(vars);
        else
            structfields = fieldnames(vars);
            for varnum = 1:length(fieldnames(vars))
                vars.(char(tmpvarlist(varnum))) = eval(char(structfields(varnum)));
            end
        end
        
        %According to naming convention, dynamic trials are always odd-numbered
        %while static trials are even-numbered. All variables but Leg Length
        %are taken from the dynamic trials but Leg Length is from the static.
        
        %tmpvar contains data from both dynamic and static, but we need to keep
        %the the corret data for each variable.  "ia" is the index of the
        %dynamic trials (i.e. indx of 1, 3, 5,....).  Here, the code loops through the chosen variable list
        %and if Leg Length is one of them, then it will choose the
        %even-numbered data (i.e. ia+1)
        
        for x = 1:length(structfields)
            if strcmp(char(structfields(x)), 'Leg_Length') == 1
                VarVal(:,x) = tmpvar(ia+1,x);
            else
                VarVal(:,x) = tmpvar(ia,x);
            end
        end
        
        emptycell = cellfun('isempty', VarVal);
        VarVal(emptycell) = {NaN};
end

