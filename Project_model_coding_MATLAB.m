%================Prepare data=================== 
%Read in the restarunt location data
C = xlsread("Project_model_optimal_p2.xlsx", "Sheet1", "B3:C20");

%Create a 18*18 matrix to hold calculated distance from place to place
d = zeros(18,18);

%For loop to calculate actual distance from place to place using 
% package of mapping toolbox
for i = 1:18
    for j = 1:18
        d(i,j) = deg2km(distance(C(i,1), C(i,2),C(j,1),C(j,2)));
    end
end

%Convert unit from KM to MILES
d = round(d / 1.6);

%Output data to csv file
%xlswrite("distance.csv", d)

%% ================Solve TSP problem==============
%Set myData paramaters, interation 50000 times

myData.xy = C
myData.dmat = d
myData.numIter = 50000
myData.showWaitbar = 1 

%call the TSP function slove Problem
X = TSP(myData);

%In America, average 0.15 dollar per miles drive
Optimal_cost = X.minDist * 0.15;
Optimal_route = X.optRoute;
Optimal_cost;
Optiaml_route;


%% SHORTEST PATH, no additional s.t
W = xlsread('Daily_Hotel_Cost_test.xlsx','Distance','B2:S19')
%W is the adjacency matrix with weights
labels={'Denver','2','3','4','5','6','7','8','9','10','11','12','13','14','15','16','17','18'};
%define the labels of the nodes
h=view(biograph(W,labels,'ShowArrows','on','ShowWeights','on'))
%visualize the network based on the adjacency natrix
[dist,path] = graphshortestpath(sparse(W),1,10)
%find the shortest path from node 1 to node 12
%Mark the nodes and edges of the shortest path by coloring
%them red and blue and increasing the line width.
set(h.Nodes(path),'Color',[1 0.4 0.4])
%[1 0.4 0.4] is the RGB color code of red
edges = getedgesbynodeid(h,get(h.Nodes(path),'ID'));
set(edges,'LineColor',[0.25 0.25 0.9])
%[0.25 0.25 0.9] is the RGB color code of blue
set(edges,'LineWidth',3)

%% Shortest path pass through 4 restarunts
dis_matrix = xlsread("Daily_Hotel_Cost_test.xlsx", "Shortest_Path", "B2:S19");
% Minimize Objective Int binary problem
% define c
[m,n] = size(dis_matrix);
c = dis_matrix';
c = c(:);

% define int
int = (1:m*n)';

% define lb & ub
lb = zeros(m*n,1);
ub=ones(m*n,1);

% Define inequality constricts
A=zeros(m,m*n); % matrix of zeros

% Buil A, 1:m rows
for i=1:m
    %for all resoucres from 1 to m
    A(i,(i-1)*n+1:i*n)=ones(1,n);
    %A(i,:) is the ith row of inequality matrix A
end
b = [ones(m,1)];
% ===Define eqlity, inflow = outflow, x1+...+x324 = 5, x1+..+X18 =1, Southend must be End Point==
% Build Aeq with zero matrix 18*324
Aeq = zeros(m,m*n);

% update positive 1 in Aeq
for i = 2:m
    Aeq(i,(i-1)*n+1:i*n)= ones(1,n)
end
% updata negative 1 in Aeq
for i = 2:m
    for j = 1:n
        Aeq(i, i+(j-1)*n) = -1 %loop through all value should be -1
    end
end
% Set Denvor as Start Point
Aeq(1,1:m) = 1;

% Set South End as End Point
%Build beq
beq = [1;zeros(8,1);-1;zeros(8,1)];

% Add a constraint so that we pass 4 restarunt in the middle
Aeq = [Aeq;ones(1,m*n)];
beq = [beq;5];

%Slove Minimize Problem
[x,z] = intlinprog(c,int,A,b,Aeq,beq,lb,ub)
Solu = reshape (x, [n m])';

%% Set covering 800$ Budget

Hcost = xlsread("Daily_Hotel_Cost_test.xlsx", "Distance", "U2:U19");
Rest = xlsread("Daily_Hotel_Cost_test.xlsx", "Distance", "B22:S39");
[m,n]= size(Rest);

A=[-Rest,eye(m)];
% Add cost constraint
cost = [Hcost', zeros(1,18)];
% Add cost to A
A = [A;cost];

% Define b
b=zeros(m,1);
% Add cost inequlity constraint, budget set to 800;
b = [b;800];

% No Equlity
beq=[];
Aeq=[];

% Define Binary Problem
lb = zeros(2*m,1);
ub = ones(2*m,1);
int = [1:2*m]';

c=[zeros(m,1);ones(m,1)];
[u_b,z_b]=intlinprog(-c,int,A,b,Aeq,beq,lb,ub);
u_b=round(u_b) %remove unesessary decimals, if any
z_b=-z_b %this is a maximization problem
%identify the influencers
names_b=find(u_b(1:m)==1);




