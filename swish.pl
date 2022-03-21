     :-dynamic([
                       r/2,
                       breeze/1,
                       pit/1,
                       wumpus/1,
                       stench/1,
                       gold/1,
                       adjacentTo/2,
                       safe/2,
                       moveFromTo/3,
                       grabGold/2,
                       shootWumpus/1,
                       agentLocation/1,
                       time/1,
                       score/1,
                       glitter/1,
                       nextmove/1,
                       makePerception/1,
                       tell_KB/1,
                       updateCurrentlocation/1,
                       updatePreviouslocation/1,
                       perceiveBreeze/2,
                       perceivestench/2,
                       perceiveGlitter/2,
                       possibleP/2,
                       possibleWL/2,
                       pwumpus/2,
                       pPit/2,
                       isWumpus/2,
                       isGold/2,
                       isWumpus/2,      
                       previousLocation/1,
                       permittedAction/1,
                       isVisited/1,
                       chooseTarget/1,
                       stenchloc/2,
                       istinky/2,
                       scream/2,
                       preshootWumpus/0,
                       chooseTarget/2,
                       arrow/1,
                       alive/1,
                       start/0,
                       updateTime/1,
                       updateScore/1,
                       getAdjacentRooms/2,
                       perceive/1,
                       previousLocation/1,
                       pmove/1,
                       pickMove/1,
                       permitted/2,
                       explore/1,
                       goldgrabbbed/1,
                       record/2,
                       positionAndTime/2,
                       visited/1,
                       backtrack/3,
                       updateScores/0
                        ]).

                wumpus(r(3,2)).
                pit(r(4,1)).
                pit(r(4,3)).
                pit(r(2,4)).
                gold(r(1,3)).
                previousLocation(r(1,1)).
                arrow(true).
                alive(true).
                score(50).
                time(0).
                isVisited(r(1,1)).
                goldgrabbbed(false).
                visited(r(1,1)).
                backtrack(_,_,0).


          %---------------------Start the game--------------------------

         start:-
                init,

                (   arrow(true)->  format('Game Started\n\n'),  walk ).
            walk:-
                agentLocation(Al),
                pmove(Al),
                keepMoving.

                keepMoving:-
                  (score(S),S=:= 0 ->  format('No More lives.\n\n Game Over !! \n\n'), fail);
                   (arrow(false),alive(false)->time(T),updateScores,score(NewS),format('I killed the wumpus at time ~w and score is ~w \n \n',[T,NewS]),true,!);%how to stop the execution
                   (arrow(false),alive(true)->time(T),updateScores,score(NewS),format('I did not killed the wumpus.\n\n The time ~w and score is ~w \n \n',[T,NewS]), true,!);
                   (agentLocation(Al),pit(true,Al)->time(T),updateScores,score(NewS),format('I fell in a pit at time ~w and score is ~w  .\n \n',[T,NewS]),true,!);
                   (agentLocation(Al),isWumpus(true,Al)->time(T),updateScores,score(NewS),format('I got eaten at time ~w and score is ~w . \n \n',[T,NewS]),true,!);
                   ( agentLocation(Al),isGold(true,Al),goldgrabbbed(false)->time(T),updateScores,score(NewS),format('I got the gold at time ~w . Score is ~w \n\n',[T,NewS]) ),walk;
                   (arrow(true)->  format('Keep moving you are alive\n\n'), walk). 


        %---------------------UpdateScores---------------------------------------------
        updateScores:-
           ( arrow(false)-> (score(S),NewS is (S-10),retractall(score(_)),asserta(score(NewS))) );
           ( agentLocation(Al), pit(true,Al)-> (score(S),NewS is (S-1000),retractall(score(_)),asserta(score(NewS))) );
           ( agentLocation(Al),isWumpus(true,Al)->(score(S),NewS is (S-1000),retractall(score(_)),asserta(score(NewS))) ); 
           ( agentLocation(Al),isGold(true,Al),goldgrabbbed(false)->(score(S),NewS is S+1000,retractall(score(_)),asserta(score(NewS)),retractall(goldgrabbbed(_)),
                   asserta(goldgrabbbed(true)) ) ).




            %------------------------Make a move-------------------------------------------------
           
            pmove(Loc):-
                 getAdjacentRooms(Loc,List),
                 move(List).
    
            move([]):-  preshootWumpus;  ( agentLocation(Al), previousLocation(Pl),backtrack(Pl,Al,B), B<1 -> updatebacktrack,time(T),moveFromTo(Al,Pl,T) )  ;
       
            (   format('I Am STUCK!!!!!!!\n\n I will take a random move\n\n'),
              (agentLocation(Al),getAdjacentRooms(Al,List),pickRandomMove(List)),true).
              %When the agent is stuck pick a random move
            move([H|Tail]):-

            (  permitted(true,H),\+visited(H),pwumpus(false,H),pPit(false,H)->agentLocation(Al),time(T),retractall(backtrack(Al,H,_)),asserta(backtrack(Al,H,0)),retractall(backtrack(H,Al,_)),asserta(backtrack(H,Al,0)),moveFromTo(Al,H,T) ) ;
           


              move(Tail).
           
            updatebacktrack:-
            agentLocation(Al), 
            previousLocation(Pl),
            backtrack(Pl,Al,B),
                NewB is B+1,
                    retractall(backtrack(Pl,Al,B)),
                    asserta(backtrack(Pl,Al,NewB)).

            pickRandomMove([]):- true.
            pickRandomMove([H|Tail]):-
                permitted(true,H), \+visited(H) ->( agentLocation(Al),time(T),moveFromTo(Al,H,T));
                permitted(true,H),\+visited(H) ->( agentLocation(Al),time(T),moveFromTo(Al,H,T));
                pickRandomMove(Tail).



            moveFromTo(Al,A,T):-
              score(S),
              agentLocation(Al),
            %call check position here
               updateCurrentlocation(A),
               updatePreviouslocation(Al),
               updateScore(S),
               updateTime(T),
              score(NS),time(NT),
               agentLocation(C),
               previousLocation(PL),
               assert(visited(C)),
              record(C,NT),
              tell_KB(C),
              format('I am moving from ~w to ~w at time ~w and score ~w \n\n',[PL,C,NT,NS]).



           
          %--------------Makes Updates---------------------------------
                updateCurrentlocation(NewLocation) :-
                    retractall(agentLocation(_)),
                    assert(agentLocation(NewLocation)).  

              updatePreviouslocation(OldLocation):-
                    retractall(previousLocation(_)),
                    assert(previousLocation(OldLocation)). 
            updateTime(T):-
                    Newtime is T+1,
                    retractall(time(_)),
                    asserta(time(Newtime)).

             updateScore(S):-
                NewScore is S-1,
                retractall(score(_)),
                asserta(score(NewScore)).
             record(Loc,T):-
              assert( positionAndTime(Loc,T) ). 


            %----------------------------Tell the KB--------------------------------------------
              tell_KB(L):-
            

                   (    (permitted(true,L),perceiveBreeze(false,L)-> pitExist(false,L)); 
                        ( permitted(true,L),perceiveBreeze(true,L)-> pitExist(true,L)); true    ),

                  (  ( permitted(true,L),perceivestench(false,L)-> retractall(stenchloc(_,L)),
                        asserta(stenchloc(false,L)),wumpusExist(false,L)); %remember that there is no stench is located in L

                      ( permitted(true,L),perceivestench(true,L)-> retractall(stenchloc(_,L)),
                          asserta(stenchloc(true,L)), wumpusExist(true,L)) ;true ), %remember that the stench is located in L
                   (    
                    ( permitted(true,L),perceiveGlitter(false,L)-> goldExist(false,L));
                    ( permitted(true,L),perceiveGlitter(true,L)-> goldExist(true,L)) ;true
                   ) .

                wumpusExist(true,r(X,Y)):-
                 Z1 is Y+1,Z2 is Y-1,X1 is X+1, X2 is X-1,
                   (   \+visited(r(X,Z1)) ->possibleWL(true,r(X,Z1));visited(r(X,Z1)) ->possibleWL(false,r(X,Z1)) ),
                   (  \+visited(r(X,Z2)) ->possibleWL(true,r(X,Z2)); visited(r(X,Z2))->possibleWL(false,r(X,Z2)) ),
                   (  \+visited(r(X1,Y))->possibleWL(true,r(X1,Y));  visited(r(X1,Y))->possibleWL(false,r(X1,Y)) ),
                   (  \+visited(r(X2,Y)) ->possibleWL(true,r(X2,Y)); visited(r(X2,Y)) ->possibleWL(false,r(X2,Y)) ).

                wumpusExist(false,r(X,Y)):-
                Z1 is Y+1,Z2 is Y-1,X1 is X+1, X2 is X-1,
                     possibleWL(false,r(X,Z1)),
                     possibleWL(false,r(X,Z2)),
                     possibleWL(false,r(X1,Y)),
                     possibleWL(false,r(X2,Y)).

                pitExist(true,r(X,Y)):-
                Z1 is Y+1,Z2 is Y-1,X1 is X+1, X2 is X-1,
                    (  \+visited(r(X,Z1)) ->possibleP(true,r(X,Z1));visited(r(X,Z1)) ->possibleP(false,r(X,Z1)) ),
                   (  \+visited(r(X,Z2)) ->possibleP(true,r(X,Z2)); visited(r(X,Z2))->possibleP(false,r(X,Z2)) ),
                   (  \+visited(r(X1,Y))->possibleP(true,r(X1,Y));  visited(r(X1,Y))->possibleP(false,r(X1,Y)) ),
                   (  \+visited(r(X2,Y)) ->possibleP(true,r(X2,Y)); visited(r(X2,Y)) ->possibleP(false,r(X2,Y)) ).
                pitExist(false,r(X,Y)):-
                Z1 is Y+1,Z2 is Y-1,X1 is X+1, X2 is X-1,
                     possibleP(false,r(X,Z1)),
                     possibleP(false,r(X,Z2)),
                     possibleP(false,r(X1,Y)),
                     possibleP(false,r(X2,Y)).

                goldExist(true,r(X,Y)):-
                 grabGold(true,r(X,Y)).

                goldExist(false,r(X,Y)):-
                    grabGold(false,r(X,Y)).

                %not safe
                possibleWL(false,Loc):-
                    retractall(pwumpus(_,Loc)),
                    asserta(pwumpus(false,Loc))
                  
            .
                possibleWL(true,Loc):-
                    retractall(pwumpus(_,Loc )),
                    asserta(pwumpus(true,Loc))
                  
            .
                possibleP(false,Loc):-
                    retractall(pPit(_,Loc)),
                    asserta(pPit(false,Loc)).
                  


                possibleP(true,Loc):-
                    retractall(pPit(_,Loc)),
                    asserta(pPit(true,Loc)).
                 


                safe(true,L):-
                    possibleP(false,L),
                    possibleWL(false,L).
                safe(false,L):-
                    possibleP(true,L);
                    possibleWL(true,L).

                grabGold(true,r(X,Y)):-
                    gold(r(X,Y))
                   
            .
                grabGold(false,r(X,Y)):-
                    \+gold(r(X,Y))
                   
            .

            %------------------Make Percept-------------------------------
                    pit(true,X):- pit(X).
                    pit(false,X):- \+pit(X).
                    isGold(true,Y):-gold(Y).
                    isGold(false,Y):- \+gold(Y).
                    isWumpus(true,Z):- wumpus(Z).
                    isWumpus(false,Z):- \+wumpus(Z).

                    stench(r(X,Y)):-
                        wumpus(Wl),
                        adjacentTo(Wl,r(X,Y)).
                    breeze(r(X,Y)):-
                        pit(Pl),
                        adjacentTo(Pl,r(X,Y)).
                    glitter(r(X,Y)):-
                        gold(r(X,Y)).

                    perceiveBreeze(true,r(X,Y)):-
                        breeze(r(X,Y)).
                     
                    perceiveBreeze(false,r(X,Y)):-
                        \+ breeze(r(X,Y)).
                       
                    perceivestench(true,r(X,Y)):-
                        stench(r(X,Y)).
                       
                    perceivestench(false,r(X,Y)):-
                        \+stench(r(X,Y)).
                       
                    perceiveGlitter(true,r(X,Y)):-
                        glitter(r(X,Y)).
                      
                    perceiveGlitter(false,r(X,Y)):-
                        \+glitter(r(X,Y)).
                    



            %----------------------------------------------Take the decision to shoot--------------------------------------------------------------------------
                preshootWumpus:-

                    ( agentLocation(r(X,Y)), Z1 is Y+1,(permitted(true,r(X,Z1)),perceivestench(true,r(X,Y)) ,pwumpus(true,r(X,Z1) ),adjacentTo(r(X,Z1),r(X,Y) )->  shootWumpus(r(X,Z1)) ) );
                    ( agentLocation(r(X,Y)), Z2 is Y-1,(permitted(true,r(X,Z2)),perceivestench(true,r(X,Y)) ,pwumpus(true,r(X,Z2) ),adjacentTo(r(X,Z2),r(X,Y) )->  shootWumpus(r(X,Z2)) ) );
                    (agentLocation(r(X,Y)), X1 is X+1,(permitted(true,r(X1,Y)),perceivestench(true,r(X,Y)) ,pwumpus(true, r(X1,Y) ),adjacentTo( r(X1,Y) ,r(X,Y) )->  shootWumpus(r(X1,Y))) );
                    ( agentLocation(r(X,Y)), X2 is X-1,(permitted(true,r(X2,Y)),perceivestench(true,r(X,Y)) ,pwumpus(true,r(X2,Y) ),adjacentTo(r(X2,Y) ,r(X,Y) )->  shootWumpus(r(X2,Y)) )  );
                    false.

                shootWumpus(r(X,Y)):-

                    ( retractall(arrow(_)), asserta(arrow(false)),scream(true,r(X,Y)),agentLocation(Al)->  retractall(alive(_)), asserta(alive(false)), format('You sent the arrow from  ~w to location ~w and you killed wumpus\n\n',[Al,r(X,Y)] ));
                    (retractall(arrow(_)), asserta(arrow(false)), scream(false,r(X,Y)),agentLocation(Al)-> retractall(alive(_)), asserta(alive(true)), format('You sent the arrow from  ~w to location ~w and you did not killed wumpus\n\n',[Al,r(X,Y)]) ).

                shootWumpus(false):- false. 
                scream(true,r(X,Y)):-
                   
                    wumpus( r(X,Y) ).

                scream(false,r(X,Y)):-
                  
                   \+wumpus( r(X,Y) ).
            %--------------------------------------------------------------------------------------------------

                adjacentTo(r(X,Y), r(T,Z)) :-
                    ( Z1 is Z+1,X =:= T, Y =:= Z1  );
                    (Z2 is Z-1, X =:= T, Y =:= Z2 );
                    (X1 is T+1,X =:= X1,Y =:= Z);
                    ( X2 is T-1,X =:=  X2, Y =:= Z ).

                permittedAction(r(X,Y)):-
                    X>0,Y<5,
                    X<5,Y>0.
                 permitted(true,r(X,Y)):-
                    X>0,Y<5,
                    X<5,Y>0.
                permitted(false,_):-false.

                getAdjacentRooms(r(X,Y),List):-
                Z1 is Y+1,Z2 is Y-1,X1 is X+1, X2 is X-1,
                 append([r(X,Z1), r(X,Z2),r(X1,Y),r(X2,Y)],[],List),true.

              visit(false,L):-
                  \+isVisited(L).
              visit(true,L):-
                  isVisited(L).

            init:-
                retractall( agentLocation(_) ) ,
                assert( agentLocation(r(1,1)) ),
                    tell_KB(r(1,1)),
                record(r(1,1),0).



