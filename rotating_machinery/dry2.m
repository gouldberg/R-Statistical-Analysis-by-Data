clear

format compact

load dry2

get(dry2)



% ----------
plot(dry2)



% ----------
ze = dry2(1:500)

zr = dry2(501:1000)

ze = detrend(ze)

zr = detrend(zr)


plot(ze)



% ----------
% y(t) + a1 * y(k-1) + ... + ana * y(k - na)
% = b1 * x(k - nk) + ... + bnb * x(k - nb - nk + 1) + e(k)

% delay estimate:  default na = nb = 2

delay = delayest(ze)



% ----------
% estimate delay by evaluating nk = 1 to 10 by minimizing loss funciton (in log)

V = arxstruc(ze, zr, struc(2,2,1:10))

[nn, Vm] = selstruc(V, 0)



% ----------
V = arxstruc(ze, zr, struc(1:10, 1:10, 3))

nn = selstruc(V, 0)



% ----------
th4 = arx(ze, [4 4 3])


zpplot(th4, 3)



% ----------
th2 = arx(ze, [2 2 3])


zpplot(th2, 3)


compare(zr(150:350), th2, th4)



