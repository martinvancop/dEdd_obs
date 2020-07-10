clear all; close all;

indir   = '/Users/ioulianikolskaia/Boulot/SCIENCE/RESEARCH/ICE_OPTICS/DELTA_EDDINGTON/dEdd_obs/INPUT'
infile  = 'data_obs_optics.mat'
outfile = 'data_obs_optics.txt'

zfile = [indir,'/',infile];

load(zfile);

zfile_out = [indir,'/',outfile];

A(:,1)  = doy;
A(:,2)  = lat;
A(:,3)  = lon;
A(:,4)  = temp;
A(:,5)  = Fsw0;
A(:,6)  = h_i;
A(:,7)  = h_s;
A(:,8)  = f_mp;
A(:,9)  = T_obs;
A(:,10) = albedo;

dlmwrite(zfile_out,A,'delimiter', ' ', 'precision', '%7.2f');

%save(zfile_out, s1, '-ASCII')
