clear

format compact

% ------------------------------------------------------------------------------
% load data
% ------------------------------------------------------------------------------

load dryer2;

get(u2)

get(y2


% -->
% Vector |y2|, the output, contains 1000 measurements of the thermocouple voltage
% which is proportional to the temperature in the outlet airstream.
% Vector |u2| contains 1000 input data points consisting of the voltage applied to the heater.
% The input was generated as a binary random sequence that switches from one level to the other
% with probability 0.2.
% The sample time is 0.08 seconds.

% convert to iddata
dry = iddata(y2, u2, 0.08);

get(dry)



% ----------
% give names to the input nad output channles and Time units

dry.InputName = 'Heater Voltage';
dry.OutputName = 'Thermocouple Voltage';
dry.TimeUnit = 'seconds';
dry.InputUnit = 'V';
dry.OutputUnit = 'V';


% ------------------------------------------------------------------------------
% split data
% ------------------------------------------------------------------------------

ze = dry(1:300)

plot(ze(200:300));


ze = detrend(ze);


% ------------------------------------------------------------------------------
% detrending
% ------------------------------------------------------------------------------

ze = detrend(ze);

plot(ze(200:300))


% ------------------------------------------------------------------------------
% Impulse Response Function
% ------------------------------------------------------------------------------

clf

% non-parametric (FIR) model
mi = impulseest(ze);

% with 3 standard deviations confidence region
showConfidence(impulseplot(mi), 3); 


% -->
% The shaded region marks a 99.7% confidence interval. 
% There is a time delay (dead-time) of 3 samples before the output responds to the input
% (significant output outside the confidence interval). 



% ------------------------------------------------------------------------------
% Parametric estimaion by state-space model
% ------------------------------------------------------------------------------

% The simplest way to get started on a parametric estimation routine is to build a state-space model
% where the model-order is automatically determined, using a prediction error method.

m1 = ssest(ze);

m1

get(m1)


m1.a



% ----------
% values of the state-space matrices and their 1 standard deviation uncertainties, 
% use the |idssdata| command:

[A,B,C,D,K,~,dA,dB,dC,dD,dK] = idssdata(m1)


% -->
% The uncertainties are quite large even though the model fit the estimation data well. 
% This is because the model is over-parameterized, that is, it has more free parameters than
% what could be uniquely identified. 
% The variance of parameters in such cases is not well defined.
% However this does not imply that the model is unreliable. We can plot the time- and frequency-response
% of this plot and view the variance as confidence regions as discussed next. 


% ------------------------------------------------------------------------------
% Analyzing the Estimated Model:  Bode plot
% ------------------------------------------------------------------------------

h = bodeplot(m1);

% 3 standard deviation (99.7%) confidence region
showConfidence(h, 3)


% ----------
% We can compare the responses and associated variances
% of the parametric model |m1| with that of the nonparametric model |mi|:

showConfidence(stepplot(m1,'b', mi, 'r', 3), 3) 


% ------------------------------------------------------------------------------
% Analyzing the Estimated Model:  Nyquist plot
% ------------------------------------------------------------------------------

Opt = nyquistoptions;
Opt.ShowFullContour = 'off';
showConfidence(nyquistplot(m1, Opt), 3)


% -->
% The response plots show that the estimated model |m1| is quite reliable. 


% ------------------------------------------------------------------------------
% ARX model
% ------------------------------------------------------------------------------

% a difference equation model with 2 poles, 1 zero and 3 sample delays can be obtained using the |arx|
m2 = arx(ze,[2 2 3]);

m2



% ------------------------------------------------------------------------------
% A continuous time transfer function 
% ------------------------------------------------------------------------------

% with 2 poles, one zero and 0.2 second transport delay
m3 = tfest(ze, 2, 1, 0.2)


% ------------------------------------------------------------------------------
% Validating the Estimated Model to Experimental Output
% ------------------------------------------------------------------------------

zv = dry(800:900);

% preprocess the validation data
zv = detrend(zv);


set(gcf,'DefaultLegendLocation','best')

compare(zv, m1)



% --> 
% It can be observed here that the agreement is very good. The "Fit" value
% shown is calculated as:
% |Fit = 100*(1 - norm(yh - y)/norm(y-mean(y)))|
% where |y| is the measured output (=|zv.y|), and |yh| is the output of the model |m1|.


% ----------
% Comparing Estimated Models
compare(zv,m1,'b',m2,'r',m3,'c');


%----------
% The pole-zero plots for the models

h = iopzplot(m1,'b',m2,'r',m3,'c');

showConfidence(h,3);


% ----------
% The frequency functions above that are obtained from the models can be compared with one
% that is obtained using a non-parametric spectral analysis method (|spa|):
% The |spa| command produces an IDFRD model. 

gs = spa(ze);


w = linspace(0.4,pi/m2.Ts,200);
opt = bodeoptions; opt.PhaseMatching = 'on';
bodeplot(m1,'b',m2,'r',m3,'c',gs,'g',w,opt);
legend('m1','m2','m3','gs')


% --> 
% Bode responses of |m1|, |m2| and |m3| compared against the non-parametric spectral estimation model |gs|.
% The frequency responses from the three models/methods are very close.
% This indicates that this response is reliable.


% ----------
% Nyquist plot

showConfidence(nyquistplot(m1,'b',m2,'r',m3,'c',gs,'g'),3)


% -->
% The non-parametric model |gs| exhibits the most uncertainty in response. 
