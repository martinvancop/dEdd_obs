clear all
close all

% Test plot T_obs vs T_mod

%-------------
% Import data 
%-------------
indir  = '/Users/ioulianikolskaia/Boulot/SCIENCE/RESEARCH/ICE_OPTICS/DELTA_EDDINGTON/dEdd_obs/RUN/'

%Filename = [ "T_mod_default.txt" ];
Filename = [ "dEdd_transmittance.txt" ];

infile = [indir,'/',Filename{1}]

filename = infile %'/Users/ioulianikolskaia/Boulot/SCIENCE/RESEARCH/ICE_OPTICS/DELTA_EDDINGTON/dEdd_obs/RUN/T_mod_default.txt';
startRow = 2;
formatSpec = '%7s%7s%7s%7s%7s%7s%[^\n\r]';
fileID = fopen(filename,'r');
dataArray = textscan(fileID, formatSpec, 'Delimiter', '', 'WhiteSpace', '', 'TextType', 'string', 'HeaderLines' ,startRow-1, 'ReturnOnError', false, 'EndOfLine', '\r\n');
fclose(fileID);

% Replace non-numeric text with NaN.
raw = repmat({''},length(dataArray{1}),length(dataArray)-1);
for col=1:length(dataArray)-1
    raw(1:length(dataArray{col}),col) = mat2cell(dataArray{col}, ones(length(dataArray{col}), 1));
end
numericData = NaN(size(dataArray{1},1),size(dataArray,2));

for col=[1,2,3,4,5,6]
    % Converts text in the input cell array to numbers. Replaced non-numeric text with NaN.
    rawData = dataArray{col};
    for row=1:size(rawData, 1)
        % Create a regular expression to detect and remove non-numeric prefixes and suffixes.
        regexstr = '(?<prefix>.*?)(?<numbers>([-]*(\d+[\,]*)+[\.]{0,1}\d*[eEdD]{0,1}[-+]*\d*[i]{0,1})|([-]*(\d+[\,]*)*[\.]{1,1}\d+[eEdD]{0,1}[-+]*\d*[i]{0,1}))(?<suffix>.*)';
        try
            result = regexp(rawData(row), regexstr, 'names');
            numbers = result.numbers;
            
            % Detected commas in non-thousand locations.
            invalidThousandsSeparator = false;
            if numbers.contains(',')
                thousandsRegExp = '^[-/+]*\d+?(\,\d{3})*\.{0,1}\d*$';
                if isempty(regexp(numbers, thousandsRegExp, 'once'))
                    numbers = NaN;
                    invalidThousandsSeparator = true;
                end
            end
            % Convert numeric text to numbers.
            if ~invalidThousandsSeparator
                numbers = textscan(char(strrep(numbers, ',', '')), '%f');
                numericData(row, col) = numbers{1};
                raw{row, col} = numbers{1};
            end
        catch
            raw{row, col} = rawData{row};
        end
    end
end

% Replace non-numeric cells with NaN
R = cellfun(@(x) ~isnumeric(x) && ~islogical(x),raw); % Find non-numeric cells
raw(R) = {NaN}; % Replace non-numeric cells

% Allocate imported array to column variable names
hsm = cell2mat(raw(:, 1));
him = cell2mat(raw(:, 2));
ap = cell2mat(raw(:, 3));
TdEdd = cell2mat(raw(:, 4));
Tccsm3 = cell2mat(raw(:, 5));
Tobs = cell2mat(raw(:, 6));

% Clear temporary variables
clearvars filename startRow formatSpec fileID dataArray ans raw col numericData rawData row regexstr result numbers invalidThousandsSeparator thousandsRegExp R;

%-------------
% Plot
%-------------

figure; 

subplot(1,3,1); box on; hold on; %--- Tmod vs Tobs

plot(Tobs,TdEdd, 'ksq', 'MarkerFaceColor', 'k')
plot(Tobs,Tccsm3, 'rsq', 'MarkerFaceColor', 'r')
plot( [0.01, 30.], [0.01, 30.], 'k:')

legend('dEdd', 'ccsm3')
set(gca,'Xscale', 'log')
set(gca,'Yscale', 'log')
xlim([0.01, 30.]); ylim([0.01, 30.])
xlabel('T_{obs} (%)'); ylabel('T_{mod} (%)');
set(gca, 'FontName', 'Myriad Pro')
set(gca,'FontSize',16);

subplot(1,3,2); box on; hold on; %--- Tmod vs h
plot(him,TdEdd-Tobs, 'ksq', 'MarkerFaceColor', 'k')
plot(him,Tccsm3-Tobs, 'rsq', 'MarkerFaceColor', 'r')
legend('dEdd', 'ccsm3')
%set(gca,'Yscale', 'log')
xlabel('h_i (m)'); ylabel('T_{mod}-T_{obs} (%)');
set(gca, 'FontName', 'Myriad Pro')
set(gca,'FontSize',16);

subplot(1,3,3); box on; hold on; %--- Tmod vs h
plot(hsm,TdEdd-Tobs, 'ksq', 'MarkerFaceColor', 'k')
plot(hsm,Tccsm3-Tobs, 'rsq', 'MarkerFaceColor', 'r')
legend('dEdd', 'ccsm3')
%set(gca,'Yscale', 'log')
xlabel('h_s (m)'); ylabel('T_{mod}-T_{obs} (%)');
set(gca, 'FontName', 'Myriad Pro')
set(gca,'FontSize',16);

bias_ccsm3 = nanmean( Tccsm3 - Tobs )
std_ccsm3  = nanstd( Tccsm3 - Tobs)
bias_dEdd = nanmean( TdEdd - Tobs )
std_dEdd  = nanstd( TdEdd - Tobs)

