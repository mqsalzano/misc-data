function [seasontable] = fixshoename(seasontable, unqXL, unqMAT, column)
%UNTITLED2 Summary of this function goes here
%   Detailed explanation goes here
    

for shoe_num = 1:height(unqMAT(:,column)) 
        if ismember(unqMAT(shoe_num,column),unqXL) == 0

            idx = table2array(seasontable(:,column)) == (unqMAT(shoe_num, column));
            disp(unqMAT(shoe_num,1:5))
            [s_idx, tf] = listdlg('ListString', unqXL);

            if tf == 0
                tmpshoe = input('Type Shoe Name: ', 's');
                seasontable{idx, column} = cellstr(tmpshoe);
               
            else
                seasontable{idx, column} = cellstr(unqXL(s_idx));
           
            end 
        else
        end
    end
end
