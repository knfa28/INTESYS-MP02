/* start with ?- go. */
go :- write('You will be asked a series of queries.'),
	  nl,
	  hypothesize(Disease),
      write('The patient might have '),
      write(Disease),
      nl, nl,
      treatment(Disease),
	  nl,
	  write('This may lead to '),
	  outcome(Disease),
      undo.

/* hypotheses to be tested */
hypothesize(preeclampsia) :- preeclampsia , !.
hypothesize(hyperemesis)  :- hyperemesis, !.
hypothesize(miscarriage)  :-  miscarriage, !.
hypothesize(pneumonia)	  :-  pneumonia, !.
hypothesize(gastroenteritis) :- gastroenteritis, !.
hypothesize(premature_labor) :- premature_labor, !.
hypothesize(bacterial_vaginosis) :- bacterial_vaginosis, !.
hypothesize(urinary_track_infection) :- urinary_track_infection, !.
hypothesize(placental_abruption) :- placental_abruption, !.
hypothesize(unknown).

/* treatments to be applied */
treatment(preeclampsia) :- write('The patient will need to consult a cardiologist immediately'), nl,
					  	   write('To inform you beforhand, The definitive treatment for this disease is delivery of the fetus and placenta.'), nl, !.
treatment(hyperemsis) 	:- write('The patient will need to consult a doctor immediately.'), nl,
						   write('The patient is recommended to eat dry bland foods, and undergo oral hydration.'), nl, !.
treatment(miscarriage) 	:- write('The patient will need to consult a doctor immediately.'), nl, !.
treatment(pneumonia) 	:- write('The patient will need to take oral antibiotics, needs to rest, and taking painkillers might help to resolve the disease.'), nl, !.
treatment(gastroenteritis) :- write('The patient needs to begin hydration(drinking of water) and eat some food that is rich in potassium.'), nl, !.
treatment(premature_labor) :- write('The patient needs to see a doctor immediately.'), nl, !.
treatment(bacterial_vaginosis) :- write('How to prevent BV is unclear.'), nl,
                                  write('BV is not passed through sexual contact, although it is linked with having a new or more than one sex partner.'), !.
treatment(urinary_track_infection) :- write('UTIs are treated with antibiotics, but is advisable to consult with a doctor first.'), nl, !.
treatment(placental_abruption) :- write('When the separation is minor, bed rest for a few days usually stops the bleeding. '), nl,
                                  write('Moderate cases may require complete bed rest.'), nl,
								  write('Severe cases (when more than half of the placenta separates) can require immediate medical attention and early delivery of the baby.'), nl, !.
treatment(unknown) 		:- write('The patient might have a disease that is not related w/ pregnancy.'), nl,
						   write('But due to the fact that you have different kinds of symptoms, you might want to consult a doctor immediately.'), nl, !.

/* outcomes of the diseases */
outcome(preeclampsia) :- write('seizures or convulsions which cause the patient to have strokes or possibly death.'), nl,
						 write('With controlled medicine, this may only result to the patient giving birth to a smaller baby.'), nl, !.
outcome(hyperemesis)  :- write('the loss of so much potassium that paralysis may ensue and eventually lead to death of the patient.'), nl, !.
outcome(miscarriage)  :- write('either the loss of the baby or the death of the patient due to extreme blood loss.'), nl, !.
outcome(pneumonia)    :- write('a scarred lung if the infection is not cleared immediately.'), nl,
					     write('Or the death of the patient if absolutely no treatment is done.'), nl, !.
outcome(gastroenteritis) :- write('the loss of so much potassium (through vomiting and diarrhea) that paralysis or shock circulation may ensue and eventually lead to death.'), nl, !.
outcome(premature_labor) :- write('the early delivery of the baby which may result to fetal death.'), nl, !.
outcome(bacterial_vaginosis) :- write('the increased susceptibility to STDs like HIV and other pregnancy complications.'), nl,
                                write('It may also increase rush of premature labor or miscarriage'), nl, !.
outcome(urinary_track_infection) :- write('recurrent infections (more common in women who had 3+ utis.'), nl,
                                    write('As well as permanent kidney damage and an increased risk in low birth weight or premature infants'), nl, !.
outcome(placental_abruption):- write('extreme pain on the patient, and possibly death (severe cases)'), nl, 
                               write('And possibly stillbirth, premature birth, low blood pressure or count, brain damage or death may occur to the fetus.'), nl, !.
outcome(unknown).

/* disease identification rules */
preeclampsia :- longrun,
				verify(three_consecutive_high_bp_readings),
				verify(unusual_swelling_of_diff_body_parts), !.
preeclampsia :- longrun,
				verify(headache),
				verify(high_blood_pressure),
				verify(nape_pain),
				verify(swollen_feet),
				verify(vomiting),
				verify(loss_of_vision),
				verify(abdominal_pain),
				isProblem;
				isObese.

hyperemesis :- shortterm,
			   isWeak,
			   verify(sleepy),
			   verify(grogginess),
			   verify(dry_mouth),
			   verify(no_urine_output),
			   verify(excessive_vomiting),
		       verify(cant_walk_or_stand).

miscarriage :- shortterm,
			   verify(passage_of_blood_for_3_consecutive_days),
			   verify(passage_of_meaty_material), !.
miscarriage :- shortterm,
			   (takesDrugs; takesAlcohol; smokes),
			   (lacksNutrition; hardWorker; lacksSleep),
			   isAnemic,
			   verify(fast_heart_beat),
			   verify(low_blood_pressure),
			   verify(loss_of_consciousness),
			   verify(abdominal_pain);
			   isObese.

pneumonia :- anytime,
			 (isAnemic; lacksHydration; livesDry),
			 verify(has_fever),
			 verify(has_cough),
			 isWeak,
			 verify(difficulty_in_breathing),
			 verify(has_chest_pain),
			 verify(is_spitting_blood),
			 isProblems.

gastroenteritis :- anytime,
				   verify(is_experiencing_excessive_diarrhea), !.
gastroenteritis :- anytime,
				   (isAnemic; lacksHydration; livesDry),
				   verify(has_loss_of_appetite),
				   verify(is_vommitting),
				   verify(is_experiencing_normal_diarrhea),				   
				   verify(feels_overall_body_weakness),
				   verify(low_blood_pressure),
				   verify(has_bloody_stool),
				   isProblems.

premature_labor :- longrun,
				   (lackNutrition; hardWorker; lacksSleep),
				   isAnemic,
				   verify(has_abdominal_pain),
				   verify(is_experiencing_vaginal_bleeding),
				   verify(has_passage_of_vaginal_fluids).
				   
bacterial_vaginosis :- verify(discharges_grey_or_whitish_material),
                       verify(burning_sensation_when_urinating).

urinary_track_infection :- verify(burning_sensation_when_urinating),
                           verify(frequent_urination), !,
						   verify(stomach_or_side_pain),
						   verify(fever_and_chills).

placental_abruption :- longrun,
                       verify(rapid_and_fine_uterine_contractions), !,
					   verify(vaginal_bleeding),
                       verify(abdominal_pain),
					   verify(uterine_tenderness).

/* pregnancy period classification rules */
longrun   :- verify(five_months_pregnant_onwards).
shortterm :- not(longrun), verify(four_months_less_pregnant).
anytime   :- shortterm; longrun.

/* health factors classification rules */
hardWorker :- verify(excessive_workload);
			  verify(is_stressed).
lacksSleep :- verify(lacks_sleep).
isAnemic   :- verify(pale_skin),
			  verify(fast_or_irregular_heartbeat),
			  verify(shortness_of_breath),
			  verify(chest_pain),
			  verify(dizziness).
isWeak     :- verify(body_weakness).
isProblems :- verify(has_infection);
              verify(immune_system_problems).

/* nutrition factors classification rules */
lacksHydration :- verify(lacks_hydration).
lacksNutrition :- verify(is_undernourished);
				  verify(eats_to_little).
isObese        :- verify(is_obese).
takesDrugs     :- verify(takes_unprescribed_drugs).
takesAlcohol   :- verify(drinks_alcohol).
smokes		   :- verify(smokes).

/* environmental factors classification rules */
livesWarm  :- verify(lives_someplace_warm).
livesCold  :- not(livesWarm).
livesDry   :- verify(lives_someplace_dry).
livesRainy :- not(livesDry).
anyEnvironment :- livesWarm;
				  livesCold;
				  livesDry;
			   	  livesRainy.

neg(Goal) :- Goal,!,fail.

/* how to ask questions */
ask(Question) :-
    write('Does the patient have the following attribute: '),
    write(Question),
    write('? '),
    read(Response),
    nl,
    ( (Response == yes ; Response == y)
      ->
       assert(yes(Question)) ;
       assert(no(Question)), fail).

:- dynamic yes/1,no/1.

/* How to verify something */
verify(S) :-
   (yes(S)
    ->
    true ;
    (no(S)
     ->
     fail ;
     ask(S))).

/* undo all yes/no assertions */
undo :- retract(yes(_)),fail.
undo :- retract(no(_)),fail.
undo.
