% ====================================================================
% FAMILY RELATIONSHIP RULES
% ====================================================================

% ====================================================================
% DYNAMIC PREDICATES DECLARATIONS
% ====================================================================
:- dynamic male/1.
:- dynamic female/1.
:- dynamic parent/2.
:- dynamic child/2.
:- dynamic grandparent/2.
:- dynamic grandchild/2.
:- dynamic spouse/2.
:- dynamic sibling/2.
:- dynamic uncle/2.
:- dynamic aunt/2.
:- dynamic niece/2.
:- dynamic nephew/2.
:- dynamic cousin/2.
:- dynamic relative/2.

% ====================================================================
% CORE RELATIONSHIP INFERENCE RULES
% ====================================================================

% Basic relative definition - covers all family relationships
relative(X, Y) :- parent(X, Y).
relative(X, Y) :- parent(Y, X).
relative(X, Y) :- sibling(X, Y).
relative(X, Y) :- grandparent(X, Y).
relative(X, Y) :- grandchild(X, Y).
relative(X, Y) :- uncle(X, Y).
relative(X, Y) :- aunt(X, Y).
relative(X, Y) :- cousin(X, Y).

% Gender-specific parent relationships
father(X, Y) :- parent(X, Y), male(X).
mother(X, Y) :- parent(X, Y), female(X).

% Gender-specific child relationships
son(X, Y) :- child(X, Y), male(X).
daughter(X, Y) :- child(X, Y), female(X).

% Child inference from parent relationship
child(X, Y) :- parent(Y, X), X \= Y.

% Gender-specific grandparent relationships
grandfather(X, Y) :- grandparent(X, Y), male(X).
grandmother(X, Y) :- grandparent(X, Y), female(X).

% Grandparent inference from parent chain
grandparent(X, Y) :- parent(X, P), parent(P, Y), X \= Y.

% Gender-specific grandchild relationships
grandson(X, Y) :- grandchild(X, Y), male(X).
granddaughter(X, Y) :- grandchild(X, Y), female(X).

% Grandchild inference from parent chain
grandchild(X, Y) :- parent(P, X), parent(Y, P), X \= Y.

% Gender-specific spouse relationships
husband(X, Y) :- spouse(X, Y), male(X).
wife(X, Y) :- spouse(X, Y), female(X).

% Spouse inference from shared children
spouse(X, Y) :- parent(X, C), parent(Y, C), X \= Y.

% Gender-specific sibling relationships
brother(X, Y) :- sibling(X, Y), male(X).
sister(X, Y) :- sibling(X, Y), female(X).

% Sibling inference from shared parent
sibling(X, Y) :- parent(P, X), parent(P, Y), X \= Y.

% Uncle relationship inference
uncle(X, Y) :- sibling(X, Z), parent(Z, Y), male(X).

% Aunt relationship inference
aunt(X, Y) :- sibling(X, Z), parent(Z, Y), female(X).

% Nephew relationship inference
nephew(X, Y) :- sibling(Z, Y), parent(Z, X), male(X).

% Niece relationship inference
niece(X, Y) :- sibling(Z, Y), parent(Z, X), female(X).

% Cousin relationship inference
cousin(X, Y) :- child(X, W), child(Y, Z), sibling(W, Z), X \= Y, W \= Z.

% ====================================================================
% CONTRADICTION DETECTION RULES
% ====================================================================

% Self-relationship contradictions
contradiction(self_parent) :- parent(X, X).
contradiction(self_child) :- child(X, X).
contradiction(self_sibling) :- sibling(X, X).
contradiction(self_grandparent) :- grandparent(X, X).
contradiction(self_grandchild) :- grandchild(X, X).
contradiction(self_uncle) :- uncle(X, X).
contradiction(self_aunt) :- aunt(X, X).
contradiction(self_nephew) :- nephew(X, X).
contradiction(self_niece) :- niece(X, X).
contradiction(self_cousin) :- cousin(X, X).
contradiction(self_spouse) :- spouse(X, X).

% Circular relationship contradictions
contradiction(circular_parent) :- parent(X, Y), parent(Y, X).
contradiction(circular_grandparent) :- grandparent(X, Y), grandparent(Y, X).
contradiction(circular_child) :- child(X, Y), child(Y, X).
contradiction(circular_uncle) :- uncle(X, Y), uncle(Y, X).
contradiction(circular_aunt) :- aunt(X, Y), aunt(Y, X).
contradiction(circular_niece) :- niece(X, Y), niece(Y, X).
contradiction(circular_nephew) :- nephew(X, Y), nephew(Y, X).

% Extended circular parent relationship (3-generation loop)
contradiction(parent_grandchild) :- parent(X, Y), parent(Y, Z), parent(Z, X).

% Sibling-parent contradiction
contradiction(sibling_and_parent) :- sibling(X, Y), parent(X, Y).

% Gender contradictions
contradiction(gender_conflict) :- male(X), female(X).
contradiction(parent_gender_mismatch) :- parent(X, Y), male(X), mother(X, Y).
contradiction(parent_gender_mismatch) :- parent(X, Y), female(X), father(X, Y).
contradiction(child_gender_mismatch) :- son(X, Y), female(X), parent(Y, X).
contradiction(child_gender_mismatch) :- daughter(X, Y), male(X), parent(Y, X).
contradiction(parent_gender_mismatch) :- father(X, Y), mother(X, Y).

% Incest contradictions
contradiction(incest_parent_child) :- parent(X, Y), (husband(X, Y) ; wife(X, Y)).
contradiction(incest_sibling) :- sibling(X, Y), (husband(X, Y) ; wife(X, Y)).
contradiction(incest_grandparent) :- grandparent(X, Y), (husband(X, Y) ; wife(X, Y)).
contradiction(incest_extended) :- parent(X, Y), (uncle(Y, X) ; aunt(Y, X) ; niece(Y, X) ; nephew(Y, X)).
contradiction(incest_sibling_extended) :- sibling(X, Y), (uncle(X, Y) ; aunt(X, Y) ; niece(X, Y) ; nephew(X, Y)).

% Family relationship mismatches - Cousin conflicts
contradiction(cousin_sibling_mismatch) :- cousin(X, Y), sibling(X, Y).
contradiction(cousin_child_mismatch) :- cousin(X, Y), child(X, Y).
contradiction(cousin_parent_mismatch) :- cousin(X, Y), parent(X, Y).
contradiction(cousin_grandparent_mismatch) :- cousin(X, Y), grandparent(X, Y).
contradiction(cousin_uncle_mismatch) :- cousin(X, Y), uncle(X, Y).
contradiction(cousin_aunt_mismatch) :- cousin(X, Y), aunt(X, Y).

% Sibling relationship conflicts
contradiction(sibling_child_mismatch) :- sibling(X, Y), child(X, Y).
contradiction(sibling_parent_mismatch) :- sibling(X, Y), parent(X, Y).
contradiction(sibling_grandparent_mismatch) :- sibling(X, Y), grandparent(X, Y).
contradiction(sibling_uncle_mismatch) :- sibling(X, Y), uncle(X, Y).
contradiction(sibling_aunt_mismatch) :- sibling(X, Y), aunt(X, Y).

% Child relationship conflicts
contradiction(child_parent_mismatch) :- child(X, Y), parent(X, Y).
contradiction(child_grandparent_mismatch) :- child(X, Y), grandparent(X, Y).
contradiction(child_uncle_mismatch) :- child(X, Y), uncle(X, Y).
contradiction(child_aunt_mismatch) :- child(X, Y), aunt(X, Y).

% Parent relationship conflicts
contradiction(parent_child_mismatch) :- parent(X, Y), child(X, Y).
contradiction(parent_grandparent_mismatch) :- parent(X, Y), grandparent(X, Y).
contradiction(parent_uncle_mismatch) :- parent(X, Y), uncle(X, Y).
contradiction(parent_aunt_mismatch) :- parent(X, Y), aunt(X, Y).

% Grandparent relationship conflicts
contradiction(grandparent_parent_mismatch) :- grandparent(X, Y), parent(X, Y).
contradiction(grandparent_child_mismatch) :- grandparent(X, Y), child(X, Y).
contradiction(grandparent_uncle_mismatch) :- grandparent(X, Y), uncle(X, Y).
contradiction(grandparent_aunt_mismatch) :- grandparent(X, Y), aunt(X, Y).

% Uncle/Aunt conflicts
contradiction(uncle_aunt_mismatch) :- uncle(X, Y), aunt(X, Y).

% Too many relationships constraints
contradiction(too_many_parents) :- 
    child(X, P1), child(X, P2), child(X, P3), 
    P1 \= P2, P2 \= P3, P1 \= P3.

contradiction(too_many_grandmothers) :- 
    grandmother(P1, X), grandmother(P2, X), grandmother(P3, X), 
    P1 \= P2, P2 \= P3, P1 \= P3.

contradiction(too_many_grandfathers) :- 
    grandfather(P1, X), grandfather(P2, X), grandfather(P3, X), 
    P1 \= P2, P2 \= P3, P1 \= P3.
