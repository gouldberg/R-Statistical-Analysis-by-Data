clear
clc
load gasfurnace.txt
x=gasfurnace'; % series 1 is input, series 2 is output
x(1,:)=-x(1,:); % reverse sign of input to get positive reponse function
for i=1:2
    x(i,:)=x(i,:)-mean(x(i,:));
end
n=296;
time=[1:n];


bw=0.05; taprop=0.1; prob=0.95;
bw=0.5/6
align = [5,0];
maxlag=30;

[Spec,Freq,SpecVar,MultCohSq,PartCohSq,Gain,Phase,ImpResp,Lag]=mtsmoothspec(x,align,bw,taprop,prob,maxlag); 


figure(1)
plot(Freq,MultCohSq(1,:),'-k','linewidth',3)
hold on
plot(Freq,MultCohSq(2,:),'-k','linewidth',1.5)
plot(Freq,MultCohSq(3,:),'--k','linewidth',2)
plot(Freq,MultCohSq(4,:),'--k','linewidth',2)
xlabel('Frequency \itf','Fontsize', 18)
ylabel('Estimated squared coherency','Fontsize', 18)
axis([0,0.5,0,1])
set(gca, 'fontsize',15)
set(gca,'PlotBoxAspectRatio',[4,2,1])
hold off

figure(2)
plot(Freq,squeeze(Gain(1,1,:)),'k','linewidth',2)
hold on
plot(Freq,squeeze(Gain(2,1,:)),'k','linewidth',2)
plot(Freq,squeeze(Gain(3,1,:)),'k','linewidth',2)
hold off
xlabel('Frequency \itf','Fontsize', 18)
ylabel('Estimated Gain','Fontsize', 18)
set(gca, 'fontsize',15)
set(gca,'PlotBoxAspectRatio',[4,2,1])

figure(3)
plot(Freq,squeeze(Phase(1,1,:)),'-k','linewidth',2)
hold on
plot(Freq,squeeze(Phase(2,1,:)),'--k','linewidth',2)
plot(Freq,squeeze(Phase(3,1,:)),'--k','linewidth',2)
hold off
grid on
xlabel('Frequency \itf','Fontsize', 18)
ylabel('Estimateded phase','Fontsize', 18)
axis([0,0.5,0,2])
set(gca, 'fontsize',15)
set(gca,'PlotBoxAspectRatio',[4,2,1])


figure(4)
plot(Lag,squeeze(ImpResp(1,1,:)),'-*k','linewidth',2)
hold on
plot(Lag,squeeze(ImpResp(1,1,:))+squeeze(ImpResp(2,1,:)),'--k','linewidth',2)
plot(Lag,squeeze(ImpResp(1,1,:))-squeeze(ImpResp(2,1,:)),'--k','linewidth',2)
plot([-maxlag,maxlag],[0,0],'k','linewidth',2)
hold off
xlabel('Lag','Fontsize', 18)
ylabel('Estimated Lagged Response','Fontsize', 18)
set(gca, 'fontsize',15)
set(gca,'PlotBoxAspectRatio',[4,2,1])
axis([-10,20,-0.25,1.05])