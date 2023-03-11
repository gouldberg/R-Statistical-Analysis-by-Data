function [Spec,Freq,SpecVar,MultCohSq,PartCohSq,Gain,Phase,ImpResp,Lag,noiseSpec,noise,noiseACF]=mtsmoothspec(x,align,bw,taprop,prob,maxlag)
% forms the smoothed spectral estimate S of the (m) multiple time series x of size [m,n].
% x is an array with index 1 for the series number and index 2 for the time
% the LAST series is taken as the response to the remainder
% align is a vector of implicit alignment shifts s.
% analysis done on transform of x(t-s); results transformed back
% bw is the desired band-width of the smooth
% taprop is the tapering proportion
% prob is the size of confidence limits and acceptance region for coherencies
% Real Spec is spectrum estimate with indices 1 and 2 for the series and 3 for frequency
% Diagonals hold autospectra, superdiags co-spectra, subdiags quad-spectra
% Order of transform N is determined from series length
% The range of frequency Freq is 0 to 0.5 with intervals of 1/N
% SpecVar has first component the coefficient of variation of the estimated spectrum as fn of freq
% Second component is the equivalent degrees of freedom
% Third and fourth components are lower and upper multiplicative limits for univ. spectra.
% MultCohSq is the squared multiple coherency between series m and the rest
% 1st component is the estimate, 2nd is sign. limit, 3rd, 4th are low,upp limits
% PartCohSq is the squared partial coherency of series m with each of others
% Same components as MultCoh, but second index for other series, third for freq
% Gain is estimated gain of series m from each of others
% 1st component is estimates, 2nd and 3rd are low,upp limits, second index for series
% Phase is estimated phase of series m from each of others
% 1st component is estimates, 2nd and 3rd are low,upp limits, second index for series
% ImpResp is estimated impulse response from -maxlag to +maxlag
% Second component is significance limit
% Lag is vector of lags for the impulse response
% noiseSpec is the estimated spectrum of the noise process
% noise is an estimate of the noise process
% noiseACF is an estimate of the noise acf
%
% get dimensions and set transform order to a nice round number
Lag=[-maxlag:maxlag];
[m,n]=size(x);
mal=max(abs(align));
ms1=m-1;
N=5*(n+mal); % transform order
a=10^floor(log10(N));b=floor(N/a);
if b==3; b=4; end
if b==6 | b==7; b=5; end
if b==9; b=10; end
N=a*b;
% N=2*N
L=1+N/2; % length of saved spectral structures
% construct and apply cosine bell taper
taper=ones(1,n);
K=floor(n*taprop/2)+1;
tapsub=[1:K];
taper(tapsub)=0.5*(1-cos(tapsub*pi/K));
taper(n+1-tapsub)=taper(tapsub);
taper=taper/sqrt(sum(taper.^2)/n);
% plot(taper)
xtap=x.*(ones(m,1)*taper);
% plot(xtap')
% form harmonic contrasts and raw spectrum
xcont=conj(fft(xtap,N,2));
ncont=zeros(0,N); % to hold noise contrasts
% apply implicit shifts
imp=zeros(1,N);
imp(1)=1;
timp=zeros(m,N);
for i=1:m
   simp=circshift(imp,[0,align(i)]);
   timp(i,:)=conj(fft(simp));
end    
rawspec=zeros(m,m,N);
xcontis=xcont.*timp; % implicit shifts applied
for i=1:m
    for j=1:m
        rawspec(i,j,:)=squeeze(xcontis(i,:)).*conj(squeeze(xcontis(j,:)));
    end
end

% determine smoothing window using bandwidth as FD span
wm=0.9588; % bandwidth modification factor to improve approximation in next line
M=1+floor(N*wm*bw/2); % final window is approx. equiv. to uniform of width bw in [0,0.5]
% N,M;
winfd=zeros(1,N);
winfd(1:M)=1/M;
winlag=abs(fft(winfd)).^4; % lag window
format long
% winlag(1:10)
format short
winfd=real(fft(winlag)); % FD window
% swinfd=circshift(winfd',floor(0.201*N));
% FDwin=swinfd(1:L);
% swinlag=circshift(winlag',maxlag);
% LAGwin=swinlag(1:2*maxlag+1);
% generate smooth spectrum via lag window applied to covariances
sampcov=real(ifft(conj(rawspec),N,3)/n); % values beyond lag n + max(align) will be zero
% figure(1);plot(squeeze(sampcov(2,1,:))) % before lag windowing
for k=1:n+mal
    sampcov(:,:,k)=squeeze(sampcov(:,:,k))*winlag(k); % note lag is actually k-1
end
for k=2:n+mal
    sampcov(:,:,N+2-k)=squeeze(sampcov(:,:,N+2-k))*winlag(k); % for negative lags 
end
smoospec=conj(fft(sampcov,N,3));
% compensate for alignment after smoothing
for i=1:m
    for j=1:m
        % note use of .' to avoid conjugate and to conform with elem-wise products
        smoospec(i,j,:)=squeeze(smoospec(i,j,:)).'.*conj(timp(i,:)).*timp(j,:);
        if i==j
            smoospec(i,j,:)=real(smoospec(i,j,:));
        end
    end
end

% Construct output of spectral estimates
Spec=zeros(m,m,L);
for i=1:m
    Spec(i,i,:)=real(squeeze(smoospec(i,i,1:L)));
    for j=(i+1):m
        Spec(i,j,:)=real(squeeze(smoospec(i,j,1:L)));
        Spec(j,i,:)=imag(squeeze(smoospec(i,j,1:L)));
    end
end
Freq=[0:L-1]/N;
% Calculate Variance factor of Estimate
sqtaptran=fft(taper.^2,N);
cfsqtapt=real(ifft(abs(sqtaptran).^2));
% cfsqtapt
varfac=winlag.^2.*cfsqtapt/n^2;
% format long
% [[1:N]',winlag',cfsqtapt',varfac']
% format short
varfac=real(fft(varfac));
corrkernel=varfac/varfac(1);
lagkernel=real(ifft(corrkernel));
lagkernel=lagkernel.*(lagkernel>0); % to avoid roundoff negatives when squarooted later
VarRat=varfac([[1:2:N],1]);
VRZ=VarRat(1); % Variance Ratio applicable away from frequencies zero and half.
DF=2./(VarRat+VRZ); % degrees of freedom applicable over frequencies zero to half.
% format long
% DF
% format short
DFext=[DF,DF(L-1:-1:2)];
SpecVar=zeros(4,L);
SpecVar(1,:)=VarRat;
SpecVar(2,:)=DF;
SpecVar(3,:)=DF./gchi2inv(0.5*(1+prob),DF);
SpecVar(4,:)=DF./gchi2inv(0.5*(1-prob),DF);
%
MultCohSq=zeros(4,L);
PartCohSq=zeros(4,ms1,L);
Gain=zeros(3,ms1,L);
Phase=zeros(3,ms1,L);
ImpResp=zeros(2,ms1,2*maxlag+1);
if bw<1/n
    return
end
% Choleski of smoospec
specfac=zeros(m,m,N);
invfac=specfac;
invspec=specfac;
for k=1:N
    specfac(:,:,k)=chol(smoospec(:,:,k));
    invfac(:,:,k)=inv(specfac(:,:,k));
    invspec(:,:,k)=invfac(:,:,k)*invfac(:,:,k)';
    invfac(:,:,k)=invfac(:,:,k)'; % for lower factor to extract Transfer Function
    ncont(k)=squeeze(invfac(m,:,k))*squeeze(xcont(:,k))/invfac(m,m,k); % noise series contrasts
end
noiseN=real(ifft(conj(ncont)));
noise=noiseN(1:n);
% size(noise)
% figure(100)
% plot(noise)
noiseSpec=zeros(1,N);
noiseSpec=squeeze(abs(specfac(m,m,:)).^2)';
% plot(noiseSpec)
% size(noiseSpec);
noiseACFN=real(ifft(noiseSpec));
noiseACF=noiseACFN(1:maxlag+1)/noiseACFN(1);
% size(noiseACF)
% figure(101)
% plot(noiseACF,'*')
% size(squeeze(smoospec(m,m,:))')
MCS=squeeze(1-noiseSpec'./squeeze(smoospec(m,m,:)));
noiseSpec=noiseSpec(1:L);
% figure(10)
% plot(MCS)
MultCohSq(1,:)=MCS(1:L)';
MCSlim=gfinv(prob,2*ms1,DF-2*ms1);
MCSlim=2*ms1*MCSlim./(DF-2*ms1+2*ms1*MCSlim);
MultCohSq(2,:)=MCSlim(1:L);
MCStran=squeeze(atanh(sqrt(MCS(1:L))));
Normlim=gnorminv(0.5*(1+prob),0,1./sqrt(DF-2*ms1));
MCSlow=MCStran(1:L)-Normlim';
MCSlow=tanh(MCSlow);
MCSlow=MCSlow.*(MCSlow>0);
MCSlow=MCSlow.^2;
MCSupp=MCStran(1:L)+Normlim';
MCSupp=(tanh(MCSupp)).^2;
MultCohSq(3,:)=MCSlow(1:L)';
MultCohSq(4,:)=MCSupp(1:L)';
eps=0.00001; % to allow possiblility of zero coherence
for i=1:ms1
    % Partial coherency terms
    PCS=squeeze(abs(invspec(m,i,:)).^2./real((invspec(i,i,:).*invspec(m,m,:))));
    PCSlim=gfinv(prob,2,DF-2*ms1);
    PCSlim=2*PCSlim./(DF-2*ms1+2*PCSlim);
    PartCohSq(1,i,:)=PCS(1:L);
    PartCohSq(2,i,:)=PCSlim(1:L);
    PCStran=squeeze(atanh(sqrt(PCS(1:L))));
    Normlim=gnorminv(0.5*(1+prob),0,1./sqrt(DF-2*ms1));
    PCSlow=PCStran(1:L)-Normlim';
    PCSlow=tanh(PCSlow);
    PCSlow=PCSlow.*(PCSlow>0);
    PCSlow=PCSlow.^2;
    PCSupp=PCStran(1:L)+Normlim';
    PCSupp=(tanh(PCSupp)).^2;
    PartCohSq(3,i,:)=PCSlow(1:L)';
    PartCohSq(4,i,:)=PCSupp(1:L)';
    % Gain and phase terms
    Tf=squeeze(-invfac(m,i,:)./invfac(m,m,:));
    Gn=abs(Tf(1:L));
%     Spec(1,2,:)=real(Tf(1:L));
%     Spec(2,1,:)=imag(Tf(1:L));
    Gain(1,i,:)=Gn;
    Normlim=gnorminv(0.5*(1+prob))*sqrt(0.5*(VRZ+VarRat').*(1./(squeeze(PCS(1:L))+eps)-1));
    Gainlow=Gn.*(1-Normlim);
    Gainlow=Gainlow.*(Gainlow>0);
    Gainupp=Gn.*(1+Normlim);
    Gain(2,i,:)=Gainlow(1:L);
    Gain(3,i,:)=Gainupp(1:L);
    Ph=angle(Tf(1:L));
    Ph=unwrap(Ph,1.5*pi)/(2*pi);
    Normlim=gnorminv(0.5*(1+prob))*sqrt(0.5*(VRZ-VarRat').*(1./(squeeze(PCS(1:L))+eps)-1))/(2*pi);
    Normlim=0.5+(Normlim-0.5).*(Normlim<0.5); % limits wider than 1 meaningless
    Phlow=Ph-Normlim;
    Phupp=Ph+Normlim;
    Phase(1,i,:)=Ph(1:L);
    Phase(2,i,:)=Phlow(1:L);
    Phase(3,i,:)=Phupp(1:L);
    % Impulse response terms
    Irf=real(ifft(conj(Tf)));
    Irf=circshift(Irf,maxlag);
    ImpResp(1,i,:)=Irf(1:2*maxlag+1);
    SEIrf=circshift(lagkernel',maxlag+align(i)-align(m));
    IrfVar=sum((DFext./(DFext-2*ms1))'.*(abs(Tf).^2).*(1./(squeeze(PCS)+eps)-1).*(varfac+varfac(1))')/N;
    SEIrf=sqrt(SEIrf.*IrfVar)*gnorminv(0.5*(1+prob));
    ImpResp(2,i,:)=SEIrf(1:2*maxlag+1);
end

function z = gnorminv(p,mu,sigma)
%GNORMINV Inverse of the normal cumulative distribution function (cdf).
%   X = GNORMINV(P,MU,SIGMA) finds the inverse of the normal cdf with
%   mean, MU, and standard deviation, SIGMA.
%
%   The size of X is the common size of the input arguments. A scalar input  
%   functions as a constant matrix of the same size as the other inputs.    
%
%   Default values for MU and SIGMA are 0 and 1 respectively.
%


%   References:
%      [1]  M. Abramowitz and I. A. Stegun, "Handbook of Mathematical
%      Functions", Government Printing Office, 1964, 7.1.1 and 26.2.2


if nargin < 2, mu = 0; end
if nargin < 3, sigma = 1; end

[errorcode p mu sigma] = gdistchck(3,p,mu,sigma);
if errorcode > 0
    error('Requires non-scalar arguments to match in size.');
end

% It is numerically preferable to use the complementary error function
% and norminv(p) = -sqrt(2)*erfcinv(2*p) to produce accurate results
% for p near zero.

z = (-sqrt(2)*sigma).*erfcinv(2*p) + mu;

return % from gnorminv.m

function y = ggampdf(x,a,b)
%GGAMPDF Gamma probability density function.
%   Y = GGAMPDF(X,A,B) returns the gamma probability density function with
%   shape and scale parameters A and B, respectively, at the values in X.
%   The size of Y is the common size of the input arguments. A scalar input
%   functions as a constant matrix of the same size as the other inputs.
%
%   Some references refer to the gamma distribution with a single
%   parameter.  This corresponds to the default of B = 1.
%


%   References:
%      [1] Abramowitz, M. and Stegun, I.A. (1964) Handbook of Mathematical
%          Functions, Dover, New York, section 26.1.
%      [2] Evans, M., Hastings, N., and Peacock, B. (1993) Statistical
%          Distributions, 2nd ed., Wiley.

%   Copyright 1993-2004 The MathWorks, Inc.
%   $Revision: 2.10.2.5 $  $Date: 2004/01/24 09:33:56 $

if nargin < 2
    error('stats:gampdf:TooFewInputs','Requires at least two input arguments');
elseif nargin < 3
    b = 1;
end

% Return NaN for out of range parameters.
a(a <= 0) = NaN;
b(b <= 0) = NaN;

try
    z = x ./ b;

    % Negative data would create complex values, potentially creating
    % spurious NaNi's in other elements of y.  Map them to the far right
    % tail, which will be forced to zero.
    z(z < 0) = Inf;

    % Prevent LogOfZero warnings.
    warn = warning('off','MATLAB:log:logOfZero');
    u = (a - 1) .* log(z) - z - gammaln(a);
    warning(warn);
catch
    if exist('warn','var'), warning(warn); end
    error('stats:gampdf:InputSizeMismatch',...
          'Non-scalar arguments must match in size.');
end

% Get the correct limit for z == 0.
u(z == 0 & a == 1) = 0;
% These two cases work automatically
%  u(z == 0 & a < 1) = Inf;
%  u(z == 0 & a > 1) = -Inf;

% Force a 0 for extreme right tail, instead of getting exp(Inf-Inf)==NaN
u(z == Inf & isfinite(a)) = -Inf;
% Force a 0 when a is infinite, instead of getting exp(Inf-Inf)==NaN
u(z < Inf & a == Inf) = -Inf;

y = exp(u) ./ b;

return % from ggampdf.m

function x = ggaminv(p,a,b);
%GGAMINV Inverse of the gamma cumulative distribution function (cdf).
%   X = GGAMINV(P,A,B)  returns the inverse of the gamma cdf with  
%   parameters A and B, at the probabilities in P.
%
%   The size of X is the common size of the input arguments. A scalar input  
%   functions as a constant matrix of the same size as the other inputs.    
%
%   GAMINV uses Newton's method to converge to the solution.

%   References:
%      [1]  M. Abramowitz and I. A. Stegun, "Handbook of Mathematical
%      Functions", Government Printing Office, 1964, 6.5.

%   B.A. Jones 1-12-93

if nargin<3, 
    b=1;
end

[errorcode p a b] = gdistchck(3,p,a,b);

if errorcode > 0
    error('Requires non-scalar arguments to match in size.');
end

%   Initialize X to zero.
x = zeros(size(p));

k = find(p<0 | p>1 | a <= 0 | b <= 0);
if any(k),
    tmp  = NaN;
    x(k) = tmp(ones(size(k)));
end

% The inverse cdf of 0 is 0, and the inverse cdf of 1 is 1.  
k0 = find(p == 0 & a > 0 & b > 0);
if any(k0),
    x(k0) = zeros(size(k0)); 
end

k1 = find(p == 1 & a > 0 & b > 0);
if any(k1), 
    tmp = Inf;
    x(k1) = tmp(ones(size(k1))); 
end

% Newton's Method
% Permit no more than count_limit interations.
count_limit = 100;
count = 0;

k = find(p > 0  &  p < 1 & a > 0 & b > 0);
if (~any(k(:))), return; end
pk = p(k);

% Supply a starting guess for the iteration.
%   Use a method of moments fit to the lognormal distribution. 
mn = a(k) .* b(k);
v = mn .* b(k);
temp = log(v + mn .^ 2); 
mu = 2 * log(mn) - 0.5 * temp;
sigma = -2 * log(mn) + temp;
xk = exp(gnorminv(pk,mu,sigma));

h = ones(size(pk)); 

% Break out of the iteration loop for three reasons:
%  1) the last update is very small (compared to x)
%  2) the last update is very small (compared to sqrt(eps))
%  3) There are more than 100 iterations. This should NEVER happen. 

while(any(abs(h) > sqrt(eps)*abs(xk))  &  max(abs(h)) > sqrt(eps)    ...
                                 & count < count_limit), 
                                 
    count = count + 1;
    h = (ggamcdf(xk,a(k),b(k)) - pk) ./ ggampdf(xk,a(k),b(k));
    xnew = xk - h;
    % Make sure that the current guess stays greater than zero.
    % When Newton's Method suggests steps that lead to negative guesses
    % take a step 9/10ths of the way to zero:
    ksmall = find(xnew < 0);
    if any(ksmall),
        xnew(ksmall) = xk(ksmall) / 10;
        h = xk-xnew;
    end
    xk = xnew;
end


% Store the converged value in the correct place
x(k) = xk;

if count == count_limit, 
    fprintf('\nWarning: GAMINV did not converge.\n');
    str = 'The last step was:  ';
    outstr = sprintf([str,'%13.8f'],h);
    fprintf(outstr);
end

return % from ggaminv.m

function [p,plo,pup] = ggamcdf(x,a,b,pcov,alpha)
%GGAMCDF Gamma cumulative distribution function.
%   P = GGAMCDF(X,A,B) returns the gamma cumulative distribution function
%   with shape and scale parameters A and B, respectively, at the values in
%   X.  The size of P is the common size of the input arguments.  A scalar
%   input functions as a constant matrix of the same size as the other
%   inputs.
%
%   Some references refer to the gamma distribution with a single
%   parameter.  This corresponds to the default of B = 1. 
%
%   [P,PLO,PUP] = GAMCDF(X,A,B,PCOV,ALPHA) produces confidence bounds for
%   P when the input parameters A and B are estimates.  PCOV is a 2-by-2
%   matrix containing the covariance matrix of the estimated parameters.
%   ALPHA has a default value of 0.05, and specifies 100*(1-ALPHA)%
%   confidence bounds.  PLO and PUP are arrays of the same size as P
%   containing the lower and upper confidence bounds.
%
%   See also GAMFIT, GAMINV, GAMLIKE, GAMPDF, GAMRND, GAMSTAT.

%   GAMMAINC does computational work.

%   References:
%      [1] Abramowitz, M. and Stegun, I.A. (1964) Handbook of Mathematical
%          Functions, Dover, New York, section 26.1.
%      [2] Evans, M., Hastings, N., and Peacock, B. (1993) Statistical
%          Distributions, 2nd ed., Wiley.


if nargin < 2
    error('stats:gamcdf:TooFewInputs',...
          'Requires at least two input arguments.');
elseif nargin < 3
    b = 1;
end

% More checking if we need to compute confidence bounds.
if nargout > 1
    if nargin < 4
        error('stats:gamcdf:TooFewInputs',...
              'Must provide covariance matrix to compute confidence bounds.');
    end
    if ~isequal(size(pcov),[2 2])
        error('stats:gamcdf:BadCovariance',...
              'Covariance matrix must have 2 rows and columns.');
    end
    if nargin < 5
        alpha = 0.05;
    elseif ~isnumeric(alpha) || numel(alpha) ~= 1 || alpha <= 0 || alpha >= 1
        error('stats:gamcdf:BadAlpha',...
              'ALPHA must be a scalar between 0 and 1.');
    end
end

% Return NaN for out of range parameters.
a(a <= 0) = NaN;
b(b <= 0) = NaN;
x(x < 0) = 0;

try
    z = x ./ b;
    p = gammainc(z, a);
catch
    error('stats:gamcdf:InputSizeMismatch',...
          'Non-scalar arguments must match in size.');
end
p(z == Inf) = 1;

% Compute confidence bounds if requested.
if nargout >= 2
    % Approximate the variance of p on the logit scale
    logitp = log(p./(1-p));
    dp = 1 ./ (p.*(1-p)); % derivative of logit(p) w.r.t. p
    da = dgammainc(z,a) .* dp; % dlogitp/da = dp/da * dlogitp/dp
    db = -exp(a.*log(z)-z-gammaln(a)-log(b)) .* dp; % dlogitp/db = dp/db * dlogitp/dp
    varLogitp = pcov(1,1).*da.^2 + 2.*pcov(1,2).*da.*db + pcov(2,2).*db.^2;
    if any(varLogitp(:) < 0)
        error('stats:gamcdf:BadCovariance',...
              'PCOV must be a positive semi-definite matrix.');
    end
    
    % Use a normal approximation on the logit scale, then transform back to
    % the original CDF scale
    halfwidth = -norminv(alpha/2) * sqrt(varLogitp);
    explogitplo = exp(logitp - halfwidth);
    explogitpup = exp(logitp + halfwidth);
    plo = explogitplo ./ (1 + explogitplo);
    pup = explogitpup ./ (1 + explogitpup);
end

return % from ggamcdf.m

function x = gfinv(p,v1,v2);
%GFINV   Inverse of the F cumulative distribution function.
%   X=GFINV(P,V1,V2) returns the inverse of the F distribution 
%   function with V1 and V2 degrees of freedom, at the values in P.
%
%   The size of X is the common size of the input arguments. A scalar input  
%   functions as a constant matrix of the same size as the other inputs.    

%   References:
%      [1]  M. Abramowitz and I. A. Stegun, "Handbook of Mathematical
%      Functions", Government Printing Office, 1964, 26.6.2


if nargin <  3, 
    error('Requires three input arguments.'); 
end

[errorcode p v1 v2] = gdistchck(3,p,v1,v2);

if errorcode > 0
    error('Requires non-scalar arguments to match in size.');
end

%   Initialize Z to zero.
z = zeros(size(p));
x = zeros(size(p));

k = (v1 <= 0 | v2 <= 0 | isnan(p));
if any(k(:))
   x(k) = NaN;
end

k1 = (p > 0 & p < 1 & v1 > 0 & v2 > 0);
if any(k1(:))
    z = gbetainv(1 - p(k1),v2(k1)/2,v1(k1)/2);
    x(k1) = (v2(k1) ./ z - v2(k1)) ./ v1(k1);
end

k2 = (p == 1 & v1 > 0 & v2 > 0);
if any(k2(:))
   x(k2) = Inf;
end

return % from gfinv.m

function [errorcode,varargout] = gdistchck(nparms,varargin)
%GDISTCHCK Checks the argument list for the probability functions.

errorcode = 0;
varargout = varargin;

if nparms == 1
    return;
end

% Get size of each input, check for scalars, copy to output
isscalar = (cellfun('prodofsize',varargin) == 1);

% Done if all inputs are scalars.  Otherwise fetch their common size.
if (all(isscalar)), return; end

n = nparms;

for j=1:n
   sz{j} = size(varargin{j});
end
t = sz(~isscalar);
size1 = t{1};

% Scalars receive this size.  Other arrays must have the proper size.
for j=1:n
   sizej = sz{j};
   if (isscalar(j))
      t = zeros(size1);
      t(:) = varargin{j};
      varargout{j} = t;
   elseif (~isequal(sizej,size1))
      errorcode = 1;
      return;
   end
end

return % from gdistchck.m

function x = gchi2inv(p,v);
%GCHI2INV Inverse of the chi-square cumulative distribution function (cdf).
%   X = GCHI2INV(P,V)  returns the inverse of the chi-square cdf with V  
%   degrees of freedom at the values in P. The chi-square cdf with V 
%   degrees of freedom, is the gamma cdf with parameters V/2 and 2.   
%
%   The size of X is the common size of P and V. A scalar input
%   functions as a constant matrix of the same size as the other input.   

%   References:
%      [1]  M. Abramowitz and I. A. Stegun, "Handbook of Mathematical
%      Functions", Government Printing Office, 1964, 26.4.
%      [2] E. Kreyszig, "Introductory Mathematical Statistics",
%      John Wiley, 1970, section 10.2 (page 144)


if nargin < 2, 
    error('Requires two input arguments.');
end

[errorcode p v] = gdistchck(2,p,v);

if errorcode > 0
    error('Requires non-scalar arguments to match in size.');
end

% Call the gamma inverse function. 
x = ggaminv(p,v/2,2);

% Return NaN if the degrees of freedom is not positive.
k = (v <= 0);
if any(k(:))
    x(k) = NaN;
end

return % from gchi2inv.m

function y = gbetapdf(x,a,b)
%GBETAPDF Beta probability density function.
%   Y = GBETAPDF(X,A,B) returns the beta probability density
%   function with parameters A and B at the values in X.
%
%   The size of Y is the common size of the input arguments. A scalar input
%   functions as a constant matrix of the same size as the other inputs.

%   References:
%      [1]  M. Abramowitz and I. A. Stegun, "Handbook of Mathematical
%      Functions", Government Printing Office, 1964, 26.1.33.


if nargin < 3
   error('stats:betapdf:TooFewInputs','Requires three input arguments.');
end

% Return NaN for out of range parameters.
a(a<=0) = NaN;
b(b<=0) = NaN;

% Out of range x could create a spurious NaN*i part to y, prevent that.
% These entries will get set to zero later.
xOutOfRange = (x<0) | (x>1);
x(xOutOfRange) = .5;

try
    % When a==1, the density has a limit of beta(a,b) at x==0, and
    % similarly when b==1 at x==1.  Force that, instead of 0*log(0) = NaN.
    warn = warning('off','MATLAB:log:logOfZero');
    logkerna = (a-1).*log(x);   logkerna(a==1 & x==0) = 0;
    logkernb = (b-1).*log(1-x); logkernb(b==1 & x==1) = 0;
    warning(warn);
    y = exp(logkerna+logkernb - betaln(a,b));
catch
    warning(warn);
    error('stats:betapdf:InputSizeMismatch',...
          'Non-scalar arguments must match in size.');
end

% Fill in for the out of range x values, but don't overwrite NaNs from
% nonpositive params.
y(xOutOfRange & ~isnan(a) & ~isnan(b)) = 0;

return % from gbetapdf.m

function x = gbetainv(p,a,b);
%GBETAINV Inverse of the beta cumulative distribution function (cdf).
%   X = GBETAINV(P,A,B) returns the inverse of the beta cdf with 
%   parameters A and B at the values in P.
%
%   The size of X is the common size of the input arguments. A scalar input  
%   functions as a constant matrix of the same size as the other inputs.    
%
%   BETAINV uses Newton's method to converge to the solution.

%   Reference:
%      [1]     M. Abramowitz and I. A. Stegun, "Handbook of Mathematical
%      Functions", Government Printing Office, 1964.

%   B.A. Jones 1-12-93

if nargin <  3, 
    error('Requires three input arguments.'); 
end

[errorcode p a b] = gdistchck(3,p,a,b);

if errorcode > 0
    error('Requires non-scalar arguments to match in size.');
end

%   Initialize x to zero.
x = zeros(size(p));

%   Return NaN if the arguments are outside their respective limits.
k = find(p < 0 | p > 1 | a <= 0 | b <= 0);
if any(k),
   tmp = NaN;
   x(k) = tmp(ones(size(k))); 
end

% The inverse cdf of 0 is 0, and the inverse cdf of 1 is 1.  
k0 = find(p == 0 & a > 0 & b > 0);
if any(k0), 
    x(k0) = zeros(size(k0)); 
end

k1 = find(p==1);
if any(k1), 
    x(k1) = ones(size(k1)); 
end

% Newton's Method.
% Permit no more than count_limit interations.
count_limit = 100;
count = 0;

k = find(p > 0 & p < 1 & a > 0 & b > 0);
if isempty(k)
   return;
end
pk = p(k);

%   Use the mean as a starting guess. 
xk = a(k) ./ (a(k) + b(k));


% Move starting values away from the boundaries.
if xk == 0,
    xk = sqrt(eps);
end
if xk == 1,
    xk = 1 - sqrt(eps);
end


h = ones(size(pk));
crit = sqrt(eps); 

% Break out of the iteration loop for the following:
%  1) The last update is very small (compared to x).
%  2) The last update is very small (compared to 100*eps).
%  3) There are more than 100 iterations. This should NEVER happen. 

while(any(abs(h) > crit * abs(xk)) & max(abs(h)) > crit    ...
                                 & count < count_limit), 
                                 
    count = count+1;    
    h = (gbetacdf(xk,a(k),b(k)) - pk) ./ gbetapdf(xk,a(k),b(k));
    xnew = xk - h;

% Make sure that the values stay inside the bounds.
% Initially, Newton's Method may take big steps.
    ksmall = find(xnew <= 0);
    klarge = find(xnew >= 1);
    if any(ksmall) | any(klarge)
        xnew(ksmall) = xk(ksmall) /10;
        xnew(klarge) = 1 - (1 - xk(klarge))/10;
    end

    xk = xnew;  
end

% Return the converged value(s).
x(k) = xk;

if count==count_limit, 
    fprintf('\nWarning: BETAINV did not converge.\n');
    str = 'The last step was:  ';
    outstr = sprintf([str,'%13.8f'],h);
    fprintf(outstr);
end

return % from gbetainv.m

function p = gbetacdf(x,a,b);
%GBETACDF Beta cumulative distribution function.
%   P = GBETACDF(X,A,B) returns the beta cumulative distribution
%   function with parameters A and B at the values in X.
%
%   The size of P is the common size of the input arguments. A scalar input  
%   functions as a constant matrix of the same size as the other inputs.    
%
%   BETAINC does the computational work.

%   Reference:
%      [1]  M. Abramowitz and I. A. Stegun, "Handbook of Mathematical
%      Functions", Government Printing Office, 1964, 26.5.


if nargin<3, 
   error('stats:betacdf:TooFewInputs','Requires three input arguments.'); 
end

[errorcode x a b] = gdistchck(3,x,a,b);

if errorcode > 0
   error('stats:betacdf:InputSizeMismatch',...
         'Requires non-scalar arguments to match in size.');
end

% Initialize P to 0.
if isa(x,'single')
   p = zeros(size(x),'single');
else
   p = zeros(size(x));
end

k1 = find(a<=0 | b<=0);
if any(k1)
   tmp = NaN;
   p(k1) = tmp(ones(size(k1))); 
end

% If is X >= 1 the cdf of X is 1. 
k2 = find(x >= 1);
if any(k2)
   p(k2) = ones(size(k2));
end

k = find(x > 0 & x < 1 & a > 0 & b > 0);
if any(k)
   p(k) = betainc(x(k),a(k),b(k));
end

% Make sure that round-off errors never make P greater than 1.
k = find(p > 1);
p(k) = ones(size(k));

return % from gbetacdf.m