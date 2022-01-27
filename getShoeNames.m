function [shoes] = getShoeNames(filelist)
%This functiong grabs the shoe name from the file name
%   Detailed explanation goes here
    %initials = strings(length(filelist),1);
    shoes = strings(length(filelist),1);
    
    for i = 1:length(filelist)
        idx2 = strfind(filelist(i),'_');
        
        %pause
    
        tmp2=char(filelist(i));
        if tmp2(idx2(end-3)+1:idx2(end-3)+3) == 'run'
            shoes(i) = tmp2(idx2(1)+1:idx2(end-3)-1);
        else
            shoes(i) = tmp2(idx2(1)+1:idx2(end-2)-1);
        end
        %initials(i) = tmp2(2:3);
        shoes(i) = replace(shoes(i), '_',' ');
    end
    

end

