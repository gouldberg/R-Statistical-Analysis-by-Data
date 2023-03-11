
% ------------------------------------------------------------------------------
% data:  Glass Tube Manufacturing Process
%    - This example shows linear model identification of a glass tube manufacturing process. The experiments and the data are discussed in:
%      V. Wertz, G. Bastin and M. Heet: Identification of a glass tube drawing bench. Proc. of the 10th IFAC Congress, Vol 10, pp 334-339 Paper number 14.5-5-2. Munich August 1987.
%    - the output of the process is the thickness and the diameter of the manufactured tube.
%    - the inputs are the air-pressure inside the tube and the drawing speed.
% ------------------------------------------------------------------------------

load thispe25.mat


% ----------
% The data are contained in the variable glass:
% Data has 2700 samples of one input (Speed) and one output (Thickn). The sample time is 1 sec.

glass



% ------------------------------------------------------------------------------
% split data for cross-validation
% ------------------------------------------------------------------------------

% For estimation and cross-validation purpose, split it into two halves


% Estimation data
ze = glass(1001:1500)


% Validation data
zv = glass(1501:2000,:)



% ------------------------------------------------------------------------------
% plot time series
% ------------------------------------------------------------------------------

plot(ze(101:200))



% ------------------------------------------------------------------------------
% Detrending
% ------------------------------------------------------------------------------

% Let us remove the mean values as a first preprocessing step

ze = detrend(ze)

zv = detrend(zv)



% ------------------------------------------------------------------------------
% Frequency response function
% ------------------------------------------------------------------------------

% The sample time of the data is 1 second, while the process time constants might be much slower.
% We may detect some rather high frequencies in the output. In order to affirm this, let us first compute the input and output spectra: 

sy = spa(ze(:,1,[]))

su = spa(ze(:,[],1))


% ----------
% gain

clf

plot(sy)

plog(su)


clf

spectrum(sy, su)

axis([0.024 10 -5 20])

legend({'Output','Input'})

grid on


% -->
% Note that the input has very little relative energy above 1 rad/sec while the output contains relatively larger values above that frequency.
% There are thus some high frequency disturbances that may cause some problem for the model building.



% ------------------------------------------------------------------------------
% Impulse Response Function and Estimate Delay
% ------------------------------------------------------------------------------

% We compute the impulse response, using part of the data to gain some insight into potential feedback
% and delay from input to output:
% 'negative':  automatically pick negative lags for all input/output channels of the model
% N = []:  determines the order automatically using persistence of excitation analysis on the input data

Imp = impulseest(ze, [], 'negative', impulseestOptions('RegularizationKernel', 'SE'))
% Imp = impulseest(ze, [], 'negative')

showConfidence(impulseplot(Imp, -10:30), 3)
grid on


% -->
% We see a delay of about 12 samples in the impulse response
% (first significant response value outside the confidence interval),
% which is quite substantial. Also, the impulse response is not insignificant for negative time lags. 
% This indicates that there is a good probability of feedback in the data,
% so that future values of output influence (get added to) the current inputs. 
% The input delay may be calculated explicitly using delayest: 


% ----------
delayest(ze)


% The probability of feedback may be obtained using feedback:
feedback(ze)


% -->
% Thus, it is almost certain that there is feedback present in the data. 


% ------------------------------------------------------------------------------
% Check high frequency behavior:  assess the model range
% ------------------------------------------------------------------------------

% We also, as a preliminary test, compute the spectral analysis estimate:

g = spa(ze)

showConfidence(bodeplot(g))

grid on


% -->
% We note, among other things, that the high frequency behavior is quite uncertain.
% It may be advisable to limit the model range to frequencies lower than 1 rad/s.


% ------------------------------------------------------------------------------
% ARX model by 4th order
% ------------------------------------------------------------------------------

% Parametric Models of the Process Behavior
% Let us do a quick check if we can pick up good dynamics by just computing a fourth order ARX model
% using the estimation data and simulate that model using the validation data.
% We know that the delay is about 12 seconds.

m1 = arx(ze, [4 4 12])

m1

compare(zv, m1)


% ------------
% validation data
% Inf:  Compare simulated response of sys to data.
compare(zv, m1, inf, 'Samples', 101:200)


% ------------------------------------------------------------------------------
% Decimation
% ------------------------------------------------------------------------------

% There are clear difficulties to deal with the high frequency components of the output.
% That, in conjunction with the long delay, suggests that we decimate the data by four
% (i.e. low-pass filter it and pick every fourth value):

if exist('resample','file')==2
    % Use "resample" command for decimation if Signal Processing Toolbox(TM)
    % is available.
    zd = resample(detrend(glass), 1, 4, 20)
else
    % Otherwise, use the slower alternative - "idresamp"
    zd  = idresamp(detrend(glass),4)
end
   
zde = zd(1:500)

zdv = zd(501:size(zd,'N'))


% ------------------------------------------------------------------------------
% Check Impulse Response Function for decimated data
% ------------------------------------------------------------------------------

Imp = impulseest(zde)

showConfidence(impulseplot(Imp, 200), 3)

axis([0  100  -0.05  0.01])

grid on


% -->
% We again see that the delay is about 3 samples
% (which is consistent with what we saw above; 12 second delay with sample time of 4 seconds in zde).



% ------------------------------------------------------------------------------
% 4SID (Sub Space-based State-Space model IDentification method) by n4sid
% ------------------------------------------------------------------------------
% Let us now try estimating a default model, where the order is automatically picked by the estimator.

Mdefault = n4sid(zde)


Mdefault


% ----------
compare(zdv, Mdefault)


% -->
% The estimator picked a 4th order model. 
% It seems to provide a better fit than that for the undecimated data.


% ------------------------------------------------------------------------------
% systematically evaluate what model structure and orders we can use
% ------------------------------------------------------------------------------

% First we look for the delay:
% compute loss functions for single-output ARX models
V = arxstruc(zde, zdv, struc(2, 2, 1:30))


% Select model order for single-output ARX mode
nn = selstruc(V,0)


% -->
% ARXSTRUC also suggests a delay of 3 samples which is consistent with the observations
% from the impulse response. 


% ----------
% Therefore, we fix the delay to the vicinity of 3 and test several different orders with
% and around this delay:
% Now we call selstruc on the returned matrix in order to pick the most preferred model order
% (minimum loss function, which is shown in the first row of V).

V = arxstruc(zde, zdv, struc(1:5, 1:5, nn(3)-1:nn(3)+1))

nn = selstruc(V,0)


% -->
% SELSTRUC could be called with just one input to
% invoke an interactive mode of order selection
% (nn = selstruc(V)).


% ----------
% Let us compute and check the model for the "best" order returned in variable nn:

m2 = arx(zde, nn)

compare(zdv, m2, inf, compareOptions('Samples', 21:150))


% -->
% The model m2 is about same as Mdefault is fitting the data but uses lower orders.


% ----------
% Let us test the residuals:
resid(zdv, m2)


% -->
% The residuals are inside the confidence interval region, 
% indicating that the essential dynamics have been captured by the model.


% ----------
% What does the pole-zero diagram tell us? 
clf

showConfidence(iopzplot(m2),3)

axis([ -1.1898,1.3778,-1.5112,1.5688])


% -->
% From the pole-zero plot, 
% there is an indication of pole-zero cancellations for several pairs.
% This is because their locations overlap, within the confidence regions.
% This shows that we should be able to do well with lower order models. Try a [1 1 3] ARX model: 


% ----------
% Simulation of model m3 compared against the validation data shows:
% The three models deliver comparable results.

m3 = arx(zde,[1 1 3])

compare(zdv, Mdefault, m2, m3)


% Similarly, we can compare the 5-step ahead prediction capability of the models:

compare(zdv, Mdefault, m2, m3, 5)


% -->
% As these plots indicate, a reduction in model order does not significantly reduce
% its effectiveness is predicting future values.
