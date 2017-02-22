% Expert system shell based on Luger
% To run, begin

:- dynamic known/2.

start :-
	solve(disease(X),CF),
	write('I am '),
	write(CF),
	write('% sure that you have '),
	write(X).

% solve(+,?)
solve(Goal,CF) :-
	print_instructions,
	retractall(known(_,_)),
	solve(Goal,CF,[],20).
solve(Goal,CF) :-
	\+ solve(Goal,CF,[],20),
	write('No Solution Found.'), fail.
	
print_instructions :-
	nl, write('You will be asked a series of queries.'),
	nl, write('Your response must be either:'),
	nl, write('a. A number between -100 and 100 representing'),
	nl, write(' your confidence in the truth of the query'),
	nl, write('-100 meaning not at all, 100 meaning absolutely certain,'),
	nl, write('[-20,20] if not sure'),
	nl, write('b. why'),
	nl, write('c. how(X), where X is a goal'),nl.

% --------------------------% INFERENCING SUBSYSTEM % -------------------------%
	
% solve(+,?,+,+)
% if goal is known and certainty factor is above threshold, then goal is solved
solve(Goal,CF,_,Threshold) :-
	known(Goal,CF),!,
	above_threshold(CF,Threshold).
% if goal is negated, invert certainty factor and threshold and solve
solve(not(Goal),CF,Rules,Threshold) :- !,
	invert_threshold(Threshold,New_threshold),
	solve(Goal,CF_goal,Rules,New_threshold),
	negate_cf(CF_goal,CF).
% if there are two goals, solve both and let the certainty factor be the lesser
% one
solve((Goal1,Goal2),CF,Rules,Threshold) :- !,
	solve(Goal1,CF1,Rules,Threshold),
	above_threshold(CF1,Threshold),
	solve(Goal2,CF2,Rules,Threshold),
	above_threshold(CF2,Threshold),
	and_cf(CF1,CF2,CF).
% if goal has yet to be proven and is a head of a rule, add rule to rules list
% and attempt to prove premise. certainty factor is premise's certainty factor 
% as a percentage of rule's certainty factor
solve(Goal,CF,Rules,Threshold) :-
	rule((Goal:-(Premise)),CF_rule),
	solve(Premise,CF_premise,[rule((Goal:-Premise),CF_rule)|Rules],Threshold),
	rule_cf(CF_rule,CF_premise,CF),
	above_threshold(CF,Threshold).
% if goal is a fact
solve(Goal,CF,_,Threshold) :-
	rule(Goal,CF),
	above_threshold(CF,Threshold).
% if goal is askable, ask user and add answer to KB
solve(Goal,CF,Rules,Threshold) :-
	askable(Goal),
	askuser(Goal,CF,Rules),!,
	asserta(known(Goal,CF)),
	above_threshold(CF,Threshold).

% Absolute value of T is greater than absolute value of CF
above_threshold(CF,T) :- T>=0, CF>=T.
above_threshold(CF,T) :- T<0, CF=<T.

% gets additive inverse of threshold
invert_threshold(Threshold,New_threshold) :-
	New_threshold is -1 * Threshold.

% gets additive inverse of certainty factor
negate_cf(CF,Negated_CF) :-
	Negated_CF is -1 * CF.

% gets lesser of the first two arguments and returns it as the third
and_cf(A,B,A) :- A =< B.
and_cf(A,B,B) :- B < A.

% compute certainty factor of rule
rule_cf(CF_rule,CF_premise,CF) :-
	CF is (CF_rule * CF_premise / 100).

% askuser(+,?,+)
%get user input
askuser(Goal,CF,Rules) :-
	nl,write('Query : '),
	translate(Goal), write(' ? '),
	read(Ans),
	respond(Ans,Goal,CF,Rules).
	
% respond(+,+,?,+)
% user entered valid CF
respond(CF,_,CF,_) :-
	number(CF), CF=<100, CF>= -100. 
	% no response issued because user enters a valid CF
respond(Ans,Goal,CF,Rules) :-
	number(Ans), (Ans>100; Ans< -100),
	write('Response out of range [-100,100]. Try Again'),nl,
	askuser(Goal,CF,Rules).
% response issued because user enters an invalid CF

% --------------------------% EXPLANATION SUBSYSTEM % -------------------------%

% writes rule trying to be proved
respond(why,Goal,CF,[Rule|Rules]) :-
	nl, write_rule(Rule),
	askuser(Goal,CF,Rules).
% if empty ruleset
respond(why,Goal,CF,[]) :-
	nl, write('Back to top of rule stack.'), nl,
	askuser(Goal,CF,[]).
% shows how goal was proven
respond(how(X),Goal,CF,Rules) :-
	build_proof(X,CF_X,Proof,20), !,
	nl, write('The goal '), write(X),
	write(' was concluded with certainty '), write(CF_X), write('.'), nl, nl,
	write('The proof of this is:'), nl,
	write_proof(Proof,0), nl,
	askuser(Goal,CF,Rules).
% goal is not yet proven
respond(how(X),Goal,CF,Rules) :-
	write('The truth of '), write(X), nl,
	write('is not yet known.'), nl,
	askuser(Goal,CF,Rules).
% invalid response
respond(_,Goal,CF,Rules):-
	write('Unrecognized response.'), nl,
	askuser(Goal,CF,Rules).
	
explain :- 
	build_proof(disease(X),CF_X,Proof,20), !,
	nl, write('That you have '), write(X), 
	write( ' was concluded with certainty '), 
	write(CF_X), write('.'), nl, nl,
	write('The proof of this is:'), nl,
	write_proof(Proof,0), nl.
explain :-
	write('No solution found.'), nl.
explain(X) :- 
	build_proof(X,CF_X,Proof,20), !,
	nl, write('The goal '), write(X),
	write(' was concluded with certainty '), write(CF_X), write('.'), nl, nl,
	write('The proof of this is:'), nl,
	write_proof(Proof,0), nl.
explain(X) :-
	write('The truth of '), write(X), nl,
	write('was not known.'), nl.
	
% build_proof(+,?,?)
% if goal is known
build_proof(Goal,CF,(Goal,CF:-given), Threshold) :-
	known(Goal,CF), !,
	above_threshold( CF, Threshold ).
% if opposite of goal is being proven
build_proof(not(Goal),CF, not(Proof), Threshold ) :- !,
    invert_threshold( Threshold, New_Threshold ),
	build_proof(Goal,CF_goal,Proof,New_Threshold),
	negate_cf(CF_goal,CF),
	above_threshold( CF, Threshold ).
% if more than one goal, prove both
build_proof((Goal1,Goal2),CF,(Proof1,Proof2), Threshold ) :-
	build_proof(Goal1,CF1,Proof1,Threshold),
	build_proof(Goal2,CF2,Proof2,Threshold),
	and_cf(CF1,CF2,CF),
	above_threshold( CF, Threshold ).
% if goal is a rule, prove premise
build_proof(Goal,CF,(Goal,CF:-Proof),Threshold) :-
	rule((Goal:-Premise),CF_rule),
	build_proof(Premise,CF_premise,Proof,Threshold),
	rule_cf(CF_rule,CF_premise,CF),
	above_threshold( CF, Threshold ).
% if goal is a fact
build_proof(Goal,CF,(Goal,CF:-fact),Threshold) :-
	rule(Goal,CF),
	above_threshold( CF, Threshold ).

% write rule
write_rule(rule((Goal:-(Premise)),CF)) :-
	write('I am trying to prove the following rule:'), nl,
	translate(Goal), write(' if '), nl,
	write_premise(Premise),
	write('CF = '), write(CF), nl.
% write goal
write_rule(rule(Goal,CF)) :-
	write('I am trying to prove the following goal:'), nl,
	translate(Goal),
	write('CF = '), write(CF), nl.

% write multiple premises
write_premise((Premise1,Premise2)) :- !,
	write_premise(Premise1),
	write_premise(Premise2).
% write negation of premise
write_premise(not(Premise)) :- !,
	write('     '), write(not), write(' '), write(Premise), nl.
% write actual premise
write_premise(Premise) :- !,
	write('     '), translate(Premise), nl.
	
% write_proof(+,+)
% user given value
write_proof((Goal,CF:-given),Level) :-
	indent(Level), translate(Goal), write(' CF='), write(CF),
	write(' was given by the user'), nl, !.
% fact in knowledge base
write_proof((Goal,CF:-fact),Level) :-
	indent(Level), translate(Goal), write(' CF='), write(CF),
	write(' was a fact in the KB'), nl, !.
% there is a premise
write_proof((Goal,CF:-Proof),Level) :-
	indent(Level), translate(Goal), write(' CF='), write(CF), write(' if '), nl,
	New_level is Level + 1,
	write_proof(Proof,New_level), !.
% negation of proof
write_proof(not(Proof),Level) :-
	indent(Level), write((not)), nl,
	New_level is Level + 1,
	write_proof(Proof,New_level), !.
% writing multiple proofs
write_proof((Proof1,Proof2),Level) :-
	write_proof(Proof1,Level),
	write_proof(Proof2,Level), !.

translate(disease(X)) :- write( 'You have '), write(X),write('.').
translate(sitchfact) :- write('Situational factors are present.').
translate(faint) :- write('You feel faint.').
translate(patientthres(0,X)) :- write('You are less than '),write(X),
                              write(' years old.').
translate(patientthres(X,0)) :- write('You are more than '),write(X),
                                write(' years old.').
translate(patientthres(X,Y)) :- write('You are '),write(X),write(' to '),
								write(Y),write(' years old.').
translate(cardiac) :- write('You have a history of cardiac disease').
translate(lostcon) :- write('You lost consciousness.').
translate(stresstrauma) :- write('You have stress related to physical trauma.').
translate(hunger) :- write('You are suffering from hunger.').
translate(lacksleep) :- write('You have lack of sleep').
translate(hightemp) :- 
	write('You have prolonged exposure to high temperatures.').
translate(dehydration) :- write('You are dehydrated.').
translate(lightheadedness) :- write('You felt lightheaded').
translate(imbalance) :- write('You feel imbalance').
translate(normexam) :- write('You had a normal neurological exam').
translate(abnormal(X)) :- write('You had an abnormal '),write(X),
                          write(' exam.').
translate(involeye) :- write('You have involuntary eye movement.').
translate(acutevertigo) :- write('You have acute vertigo with nausea').
translate(headmot) :- write('Your symptoms worsen with head motion.').
translate(brainstem) :- write('Brain stem signs are present'),
                        write(' from neurological exam').
translate(drugs) :- write('Drug/alcohol screen is positive.').
translate(X) :- write(X).
	
% no indent
indent(0).
% indent
indent(X) :-
	write('     '), X_new is X - 1, indent(X_new).

% -----------------------------% KNOWLEDGE BASE % -----------------------------%

rule((disease('Vasovagal Syncope') :- lostcon,sitchfact),100).
rule((disease('Vasovagal Syncope') :- lostcon,(((faint,patientthres(0,30)),
       not(sitchfact)),not(cardiac))),100).
rule((disease('Arythmia') :- lostcon,((cardiac,not(sitchfact)),faint)),100).
rule((disease('Cartiod sinus syndrome') :- lostcon,(((not(sitchfact),faint),
      patientthres(35,45)),not(cardiac))),100).
rule((disease('Hypotension') :- not(sitchfact),(faint,(not(cardiac),
      patientthres(55,0)))),100).

rule((disease('Visual problems/Multiple sensory defect/Cervical spine disease')
     :- lightheadedness),100).
rule((disease('Visual problems/Multiple sensory defect/Cervical spine disease')
	 :-imbalance,normexam),100).
rule((disease('Cerebellar disease') :- abnormal(cerebellar),imbalance),100).
rule((disease('Spinal cord disease') :- abnormal(reflex),(((abnormal(motor),
      imbalance),abnormal('autonomic function')),abnormal(sensory))),100).
rule((disease('Mild hemaparesis or paraparesis') :- abnormal(motor),imbalance),
      100).
rule((disease('Basal ganglion disease') :- abnormal(motor),imbalance),100).
rule((disease('Drugs/psychogenic symptoms/mild vestibulopathy'):- 
      not(involeye),(acutevertigo,not(headmot))),100).
rule((disease('Brain stem CVA/TIA/Multiple sclerosis/cerebellar lesion'):- 
      brainstem,(acutevertigo,headmot)),100).
rule((disease('Toxic vestibulopathy') :- normexam,(not(headmot),(acutevertigo,
      (drugs,involeye)))),100).
	  
rule((sitchfact :- stresstrauma),100).
rule((sitchfact :- hunger),100).
rule((sitchfact :- lacksleep),100).
rule((sitchfact :- hightemp),100).
rule((sitchfact :- dehydration),100).

rule((normexam :- not(abnormal(cerebellar)),(not(abnormal(reflex)),
     (not(abnormal(motor)),(not(abnormal('autonomic function')),
	 not(abnormal(sensory)))))),100).

askable(lostcon).
askable(stresstrauma).
askable(hunger).
askable(lacksleep).
askable(hightemp).
askable(dehydration).
askable(patientthres(_,_)).
askable(cardiac).
askable(faint).
askable(abnormal(_)).
askable(lightheadedness).
askable(imbalance).
askable(involeye).
askable(headmot).
askable(acutevertigo).
askable(drugs).