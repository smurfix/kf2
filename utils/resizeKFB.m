function resizeKFB(fnin,fnout,scale)

meth = 'bicubic'; 

% read kfb file and scale by factor 1/scale

info = dir(fnin);
fid = fopen(fnin,'r');
d = fread(fid, [1, info.bytes], '*uint8');
fclose(fid);
fid = fopen(fnout,'w');
%d(1:3) == 'KFB' test skipped
fwrite(fid,d(1:3),'uint8');
k = 4;
nBytes = 4;
width = typecast(uint8(d(k:k+nBytes-1)),'uint32');
width2 = width/scale;
width2 = typecast(uint32(width2),'uint8');
fwrite(fid,width2,'uint8');

k = k + nBytes;
height = typecast(uint8(d(k:k+nBytes-1)),'uint32');
height2 = height/scale;
height2 = typecast(uint32(height2),'uint8');
fwrite(fid,height2,'uint8');

k = k + nBytes;
nCounts = double(width)*double(height); %32 bit ints
nBytes = 4 * nCounts;
counts = typecast(uint8(d(k:k+nBytes-1)),'uint32');
counts = reshape(counts,height,width);
counts2 = exp(imresize(log(single(counts)),1/scale,meth));
clear counts;
counts2 = typecast(uint32(counts2(:)),'uint8');
fwrite(fid,counts2,'uint8');
clear counts2;

k = k + nBytes;
nBytes = 4;
iterdiv = typecast(uint8(d(k:k+nBytes-1)),'uint32');
fwrite(fid,d(k:k+nBytes-1),'uint8');

k = k + nBytes;
parts = typecast(uint8(d(k:k+nBytes-1)),'uint32');
fwrite(fid,d(k:k+nBytes-1),'uint8');
parts = double(parts);

k = k + nBytes;
nBytes = parts * 3;
% r,g,b,... parts triples
keys = typecast(uint8(d(k:k+nBytes-1)),'uint8');
fwrite(fid,d(k:k+nBytes-1),'uint8');
keys = double(keys);

k = k + nBytes;
nBytes = 4;
maxiterInt32 = typecast(uint8(d(k:k+nBytes-1)),'uint32');
fwrite(fid,d(k:k+nBytes-1),'uint8');
maxiter = double(maxiterInt32);

k = k + nBytes;
nBytes = 4 * nCounts;
trans = typecast(uint8(d(k:k+nBytes-1)),'single');
trans = reshape(trans,height,width);
trans2 = exp(imresize(log(trans),1/scale,meth));
clear trans;
trans2 = typecast(trans2(:),'uint8');
fwrite(fid,trans2,'uint8');
clear trans2;

% DE data
k = k + nBytes;
if(k>info.bytes)
    fprintf('No ADE data found\n');
else
    nBytes = 4 * nCounts;
    de = typecast(uint8(d(k:k+nBytes-1)),'single');
    de = reshape(de,height,width);
    de2 = exp(imresize(log(de),1/scale,meth));
    clear de;
    de2 = typecast(de2(:),'uint8');
    fwrite(fid,de2,'uint8');
endif

fclose(fid);

endfunction
