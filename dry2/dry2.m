clear

format compact

load dry2

get(dry2)



% ----------
% plot input and ouput data
plot(dry2)



% ----------
% split data for training and test
ze = dry2(1:500)

zr = dry2(501:1000)

% detrending
ze = detrend(ze)

zr = detrend(zr)


% test data
plot(ze)



% -------------------------------------------------------------------------
% Estimate reponse delay by Output-Error model
% -------------------------------------------------------------------------

% y(t) + a1 * y(k-1) + ... + ana * y(k - na)
% = b1 * x(k - nk) + ... + bnb * x(k - nb - nk + 1) + e(k)

% delay estimate:  default na = nb = 2

delay = delayest(ze)


% -->
% estimated delay = 3


% -------------------------------------------------------------------------
% Estimate reponse delay by ARX model
% -------------------------------------------------------------------------

% estimate delay by evaluating nk = 1 to 10 by minimizing loss funciton (in log)
% by cross-validation

struc(1:10, 1:10, 1:10)
 
% set training and test data
V = arxstruc(ze, zr, struc(1:10, 1:10, 1:10))

[nn, Vm] = selstruc(V, 0);

nn

% -->
% na = 10, nb = 5, nk = 3 achieves smallest loss value



% -------------------------------------------------------------------------
% Here We try ARX model with na = 4, nb = 4, nk = 3
% -------------------------------------------------------------------------

th4 = arx(ze, [4 4 3])


% ----------
% check zero and poles in Re-Im plane

zpplot(th4, 3)


% -->
% cross-over suggesting cancellation


% -------------------------------------------------------------------------
% Here We try ARX model with na = 2, nb = 2, nk = 3
% -------------------------------------------------------------------------

th2 = arx(ze, [2 2 3])


zpplot(th2, 3)


% -------------------------------------------------------------------------
% model comparison
% -------------------------------------------------------------------------

compare(zr(150:350), th2, th4)


% -->
% almost same fitting

