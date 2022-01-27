function [filelist,testshoe,TS_version] = grabfilename(filepathlist)
%UNTITLED Summary of this function goes here
%   Detailed explanation goes here
    filelist = strings(length(filepathlist),1);
    testshoe = strings(length(filepathlist),1);
    TS_version = strings(length(filepathlist),1);
    
    
    for i = 1:length(filepathlist)
        idx = strfind(filepathlist(i),'\');
        tmp1=char(filepathlist(i));
        filelist(i) = tmp1(idx(end):end);
        testshoe(i) = tmp1(idx(end-4)+1:idx(end-3)-1);
        TS_version(i) = tmp1(idx(end-5)+1:idx(end-4)-1);
    end
    
end

