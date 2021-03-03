
clear

global Sa Sna S10 S20 t1l t2l

%% REMINDER: Use hours 3-8 to estimate c12 and c21 by fitting, rather than direct temporal estimation

% Using fit_logistics.m from https://www.mathworks.com/matlabcentral/fileexchange/41781-fit_logistic-t-q

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%5
%Simulation Data
%Data derived from supernatant experiments performed with sGFP S. aureus,
%1850
%% Growth rates of monocultures in fresh media (enter from exp. data)
r1 = 1.85; %species 1 - Sa - from AA data 09JUL2020
r2 = 1.56; %species 2 - Sna1850 - from AA data 09JUL2020
%% Carrying capacities of monocultures in fresh media (enter from exp. data)
K1 = 0.61; %species 1 - Sa - from AA data 09JUL2020
K2 = 0.35; %species 2 - Sna1850 - from AA data 09JUL2020

%% Carrying capacities in supernatants (enter from exp. data)
K12 = 0.17; % species 1 in supernatant of species 2
K21 = 0.03; % species 2 in supernatant of species 1

%% Interaction coefficients
c12 = -1/K2*(K1-K12);
c21 = -1/K1*(K2-K21);

% initial ODs
S10 = 0.0025;
S20 = 0.0025;

%% Simulate the dynamics
dt = 10/60; % time step, hrs
trng = dt*(1:96); %duration, time-steps
S1 = zeros(size(trng));
S2 = zeros(size(trng));

%% Assuming tl hour lag time at dilution step
t1l = 2.0; %2.35; %Sa - from AA data 09JUL2020
t2l = 1.6; %2.25; %Sna1850 - from AA data 09JUL2020

S1l(1) = S10;
S2l(1) = S20;

cnt = 1;
for t = trng(2:length(trng))
    cnt = cnt+1;
    if t>t1l
        S1l(cnt) = S1l(cnt-1) + dt*r1*S1l(cnt-1)*(1-(S1l(cnt-1)-c12*S2l(cnt-1))/K1);
    else
        S1l(cnt) = S1l(cnt-1);
   
    end
    
    if t>t2l
        S2l(cnt) = S2l(cnt-1) + dt*r2*S2l(cnt-1)*(1-(S2l(cnt-1)-c21*S1l(cnt-1))/K2);
    else
        S2l(cnt) = S2l(cnt-1);
    end
end

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%Experimental Data

% select the data file
% [infile,inpath] = uigetfile('Z:\Members\Babak\Experiments\Analysis\Sandra\Coculture fluorescence\*.txt');
inpath = 'C:\Users\momen\Google Drive\Modeling\Habitat quality\Co-culture Data\Growth Curve Data\';
infile = 'Cocultures_GFPSa_1850_1821_MC_Day1_29JUL2019.txt';
fid = fopen(strcat(inpath,infile),'r');

ch = '';

n = 0;
N = 96; %initially planned number of time points
Nr = 96; %max time actual reads from text file
dt = 10/60; % measurement time-step (hours)

r = 8; % number of rows
c = 12; % number of columns
Rd = zeros(1,Nr);
OD6 = zeros(r,c,Nr); % all OD values
GFP = zeros(r,c,Nr); % all GFP values
DsRed = zeros(r,c,Nr); % all DsRed values
ch = fscanf(fid,'%s',1);
while strcmp(ch,'Temperature')==0
    ch = fscanf(fid,'%s',1);
end
while strcmp(ch,'Time')==0
    ch = fscanf(fid,'%s',1);
end 
while n < Nr
    n = n+1;
    Rd(n) = fscanf(fid,'%i',1);
    Ts = fscanf(fid,'%s',1);
    Time(n,1:length(Ts)-2) = Ts(2:length(Ts)-1);
    ch = fscanf(fid,'%s',3);
    ch = fscanf(fid,'%s',13);
    for i = 1:r
        for j = 1:c
            OD6(i,j,n) = str2double(fscanf(fid,'%s',1));
        end
        ch = fscanf(fid,'%s',4);
    end
    ch = fscanf(fid,'%s',3);
end
while strcmp(ch,'Temperature:')==0
    ch = fscanf(fid,'%s',1);
end

while strcmp(ch,'Time')==0
    ch = fscanf(fid,'%s',1);
end
n = 0;
while n < Nr
    n = n+1;
    Rd(n) = fscanf(fid,'%i',1);
    Ts = fscanf(fid,'%s',1);
    TimeG(n,1:length(Ts)-2) = Ts(2:length(Ts)-1);
    ch = fscanf(fid,'%s',3);
    ch = fscanf(fid,'%s',13);
    for i = 1:r
        for j = 1:c
            GFP(i,j,n) = str2double(fscanf(fid,'%s',1));
        end
        ch = fscanf(fid,'%s',4);
    end
    ch = fscanf(fid,'%s',3);
end
for ndump = Nr+1:N
    ch = fscanf(fid,'%s',53);
end

ch=fclose(fid);

% Conversion of GFP to OD and DsRed to OD
% GFPtoOD = (GFP - mean(mean(GFP(1,1:8,1:10))))/192970;

%% Testing coculture
trng = 10/60*(1:Nr);
repl = 2:7; 

%1:1 Sa:Sna
col = 5;

%S. aureus OD - Exp 
SaOD = GFPtoOD(shiftdim(GFP(repl,col,1:Nr),2)-(mean(GFP(repl,col,1:5),3)*ones(1,Nr))');
%Species 2 OD - Exp
TotalOD = shiftdim(OD6(repl,col,1:Nr),2)-(mean(OD6(repl,col,1:5),3)*ones(1,Nr))';
SnaOD = TotalOD - SaOD;

S1e = nanmean(SaOD,2)'; % smooth(nanmean(SaOD,2)',3)';
S1e(1) = 0;
S2e = nanmean(TotalOD,2)'-S1e; %smooth(nanmean(TotalOD,2)'-S1e,3)'; 
S2e(1) = 0;

Sa = S1e(1:96);
Sna = S2e(1:96);

initial_c = [-1.25 -0.5];
opt_c = lsqnonlin(@func_coculture_growth_SaSna,initial_c);

disp(opt_c)
c12o = opt_c(1);
c21o = opt_c(2);

S1lo(1) = S10;
S2lo(1) = S20;

cnt = 1;
for t = trng(2:length(trng))
    cnt = cnt+1;
    if t>t1l
        S1lo(cnt) = S1lo(cnt-1) + dt*r1*S1lo(cnt-1)*(1-(S1lo(cnt-1)-c12o*S2lo(cnt-1))/K1);
    else
        S1lo(cnt) = S1lo(cnt-1);
   
    end
    
    if t>t2l
        S2lo(cnt) = S2lo(cnt-1) + dt*r2*S2lo(cnt-1)*(1-(S2lo(cnt-1)-c21o*S1lo(cnt-1))/K2);
    else
        S2lo(cnt) = S2lo(cnt-1);
    end
end

figure
%S. aureus OD - Simulated 
plot(trng,SaOD,'color',[0.2 0.4 0.7])
hold on
%Species 2 OD - Simulated 
plot(trng,SnaOD,'color',[1 0.4 0.2])
%S. aureus OD - Simulated 
plot(trng,S1l,'b:') % lag time
%Species 2 OD - Simulated 
plot(trng,S2l,'r:') % lag time
plot(trng,S1lo,'b--') % lag time, fitted
plot(trng,S2lo,'r--') % lag time, fitted
xlabel('Time (hrs)')
ylabel('OD')
xlim([2 16])


% Estimated using difference of logs (better estimate for exp)
c12el(1:cnt-1) = 1./S2e(1:cnt-1).*(S1e(1:cnt-1) - K1*(1-1/r1/dt*diff(log(S1e))));
c21el(1:cnt-1) = 1./S1e(1:cnt-1).*(S2e(1:cnt-1) - K2*(1-1/r2/dt*diff(log(S2e))));
% Estimated using temporal differences (better estimate for sim)
% c12ea(1:cnt-1) = 1./S2e(1:cnt-1).*(S1e(1:cnt-1) - K1*(1-1/r1/dt*diff(S1e)./S1e(1:cnt-1)));
% c21ea(1:cnt-1) = 1./S1e(1:cnt-1).*(S2e(1:cnt-1) - K2*(1-1/r2/dt*diff(S2e)./S2e(1:cnt-1)));

% plot estimated interaction coefficients for experimental results
figure
plot(trng(1:cnt-1),c12el,'bo','MarkerSize',4)
hold on
plot(trng(1:cnt-1),c21el,'ro','MarkerSize',4)
plot([min(trng) max(trng)],[c12 c12],'b:')
plot([min(trng) max(trng)],[c21 c21],'r:')
plot([min(trng) max(trng)],[c12o c12o],'b--')
plot([min(trng) max(trng)],[c21o c21o],'r--')
xlabel('Time (hrs)')
ylabel('Interaction coefficient')
xlim([6 16])
ylim([-2 0])

% plot estimated interaction coefficients over OD
figure
plot(S1l(1:cnt-1),c12*ones(1,cnt-1),'b:')
hold on
plot(S1e(1:cnt-1),c12el,'bo','MarkerSize',4)
plot(S1lo(1:cnt-1),c12o*ones(1,cnt-1),'b--')
legend('Model','Exp','Exp-Fit')
xlabel('Population size, Sa (OD)')
ylabel('Interaction coefficient')
xlim([0.0 0.15])
ylim([-3 2])

figure
plot(S2l(1:cnt-1),c21*ones(1,cnt-1),'r:')
hold on
plot(S2e(1:cnt-1),c21el,'ro','MarkerSize',4)
plot(S2lo(1:cnt-1),c21o*ones(1,cnt-1),'r--')
legend('Model','Exp','Exp-Fit')
xlabel('Population size, Sna (OD)')
ylabel('Interaction coefficient')
xlim([0.0 0.5])
ylim([-3 2])
