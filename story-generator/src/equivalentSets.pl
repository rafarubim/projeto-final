:- module(myModule, [equivalentSets/2]).

:- use_module(set).

:- dynamic setFact/1.

printTime(Term) :-
  statistics(walltime, _),
  Term,
  statistics(walltime, [_, Time]),
  write(Time).

setSubsetOf(SetA, SetB) :-
  assertSet(SetB),
  testSet(SetA),
  cleanup.

equivalentSets(SetA, SetB) :-
  setSubsetOf(SetA, SetB),
  setSubsetOf(SetB, SetA).

% private

assertSet([]).
assertSet([H|T]) :-
  assert(setFact(H)),
  assertSet(T).

testSet([]).
testSet([H|T]) :-
  setFact(H),
  testSet(T).

cleanup :-
  retractall(setFact(_)).

hashTest :-
  printTime(
    equivalentSets(
    [meat,student,bee,sale,motion,nine,taken,lose,becoming,cowboy,stopped,share,west,more,rhythm,couple,doll,globe,continent,adjective,industry,nearer,motor,settle,circus,material,paragraph,largest,deep,cast,evidence,post,rate,dark,key,stuck,chair,oldest,condition,record,scientific,brown,worth,valley,adventure,hurried,prize,seems,traffic,stretch,strength,warm,almost,food,from,price,be,bigger,took,stage,question,easily,given,driven,base,world,primitive,stairs,born,jar,rhyme,quarter,gift,state,deal,prove,guard,spider,body,serious,remain,journey,discovery,fill,production,natural,evening,my,under,eleven,spring,model,balance,sad,step,sunlight,we,essential,usually,selection,will,living,leg,child,statement,shop,onto,wheat,yellow,hat,shut,repeat,horn,free,corner,equipment,century,involved,final,western,has,teach,thousand,exist,fourth,canal,small,swung,force,pull,means,already,supply,habit,cent,mental,actually,parent,jump,advice,say,realize,thick,all,center,will,wrapped,mixture,they,brick,love,ourselves,twelve,daily,combination,repeat,cost,increase,land,little,farmer,six,possibly,graph,create,camera,dollar,sentence,everybody,basis,buried,few,dog,cat,announced,supper,storm,many,cookies,blood,no,near,word,teach,again,pool,chicken,nearer,beneath,dropped,tie,sheep,built]
    ,
    [sunlight,we,essential,usually,selection,will,living,leg,child,statement,shop,onto,wheat,yellow,hat,shut,repeat,horn,free,corner,equipment,century,involved,final,western,has,teach,thousand,exist,fourth,canal,small,swung,force,pull,means,already,supply,habit,cent,mental,actually,parent,jump,advice,say,realize,thick,all,center,will,wrapped,mixture,they,brick,love,ourselves,twelve,daily,combination,repeat,cost,increase,land,little,farmer,six,possibly,graph,create,camera,dollar,sentence,everybody,basis,buried,few,dog,cat,announced,supper,storm,many,cookies,blood,no,near,word,teach,again,pool,chicken,nearer,beneath,dropped,tie,sheep,built,meat,student,bee,sale,motion,nine,taken,lose,becoming,cowboy,stopped,share,west,more,rhythm,couple,doll,globe,continent,adjective,industry,nearer,motor,settle,circus,material,paragraph,largest,deep,cast,evidence,post,rate,dark,key,stuck,chair,oldest,condition,record,scientific,brown,worth,valley,adventure,hurried,prize,seems,traffic,stretch,strength,warm,almost,food,from,price,be,bigger,took,stage,question,easily,given,driven,base,world,primitive,stairs,born,jar,rhyme,quarter,gift,state,deal,prove,guard,spider,body,serious,remain,journey,discovery,fill,production,natural,evening,my,under,eleven,spring,model,balance,sad,step]
    )  
  ).

forceTest :-
  printTime(
    equivalentTo(
    [meat,student,bee,sale,motion,nine,taken,lose,becoming,cowboy,stopped,share,west,more,rhythm,couple,doll,globe,continent,adjective,industry,nearer,motor,settle,circus,material,paragraph,largest,deep,cast,evidence,post,rate,dark,key,stuck,chair,oldest,condition,record,scientific,brown,worth,valley,adventure,hurried,prize,seems,traffic,stretch,strength,warm,almost,food,from,price,be,bigger,took,stage,question,easily,given,driven,base,world,primitive,stairs,born,jar,rhyme,quarter,gift,state,deal,prove,guard,spider,body,serious,remain,journey,discovery,fill,production,natural,evening,my,under,eleven,spring,model,balance,sad,step,sunlight,we,essential,usually,selection,will,living,leg,child,statement,shop,onto,wheat,yellow,hat,shut,repeat,horn,free,corner,equipment,century,involved,final,western,has,teach,thousand,exist,fourth,canal,small,swung,force,pull,means,already,supply,habit,cent,mental,actually,parent,jump,advice,say,realize,thick,all,center,will,wrapped,mixture,they,brick,love,ourselves,twelve,daily,combination,repeat,cost,increase,land,little,farmer,six,possibly,graph,create,camera,dollar,sentence,everybody,basis,buried,few,dog,cat,announced,supper,storm,many,cookies,blood,no,near,word,teach,again,pool,chicken,nearer,beneath,dropped,tie,sheep,built]
    ,
    [sunlight,we,essential,usually,selection,will,living,leg,child,statement,shop,onto,wheat,yellow,hat,shut,repeat,horn,free,corner,equipment,century,involved,final,western,has,teach,thousand,exist,fourth,canal,small,swung,force,pull,means,already,supply,habit,cent,mental,actually,parent,jump,advice,say,realize,thick,all,center,will,wrapped,mixture,they,brick,love,ourselves,twelve,daily,combination,repeat,cost,increase,land,little,farmer,six,possibly,graph,create,camera,dollar,sentence,everybody,basis,buried,few,dog,cat,announced,supper,storm,many,cookies,blood,no,near,word,teach,again,pool,chicken,nearer,beneath,dropped,tie,sheep,built,meat,student,bee,sale,motion,nine,taken,lose,becoming,cowboy,stopped,share,west,more,rhythm,couple,doll,globe,continent,adjective,industry,nearer,motor,settle,circus,material,paragraph,largest,deep,cast,evidence,post,rate,dark,key,stuck,chair,oldest,condition,record,scientific,brown,worth,valley,adventure,hurried,prize,seems,traffic,stretch,strength,warm,almost,food,from,price,be,bigger,took,stage,question,easily,given,driven,base,world,primitive,stairs,born,jar,rhyme,quarter,gift,state,deal,prove,guard,spider,body,serious,remain,journey,discovery,fill,production,natural,evening,my,under,eleven,spring,model,balance,sad,step]
    )  
  ).

%equivalentTo([a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v,w,x,y,z,a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v,w,x,y,z,a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v,w,x,y,z,a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v,w,x,y,z,a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v,w,x,y,z,a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v,w,x,y,z,a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v,w,x,y,z,a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v,w,x,y,z,a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v,w,x,y,z,a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v,w,x,y,z],[q,w,e,r,t,y,u,i,o,p,a,s,d,f,g,h,j,k,l,z,x,c,v,b,n,m,q,w,e,r,t,y,u,i,o,p,a,s,d,f,g,h,j,k,l,z,x,c,v,b,n,m,q,w,e,r,t,y,u,i,o,p,a,s,d,f,g,h,j,k,l,z,x,c,v,b,n,m,q,w,e,r,t,y,u,i,o,p,a,s,d,f,g,h,j,k,l,z,x,c,v,b,n,m,q,w,e,r,t,y,u,i,o,p,a,s,d,f,g,h,j,k,l,z,x,c,v,b,n,m,q,w,e,r,t,y,u,i,o,p,a,s,d,f,g,h,j,k,l,z,x,c,v,b,n,m,q,w,e,r,t,y,u,i,o,p,a,s,d,f,g,h,j,k,l,z,x,c,v,b,n,m,q,w,e,r,t,y,u,i,o,p,a,s,d,f,g,h,j,k,l,z,x,c,v,b,n,m,q,w,e,r,t,y,u,i,o,p,a,s,d,f,g,h,j,k,l,z,x,c,v,b,n,m,q,w,e,r,t,y,u,i,o,p,a,s,d,f,g,h,j,k,l,z,x,c,v,b,n,m]).