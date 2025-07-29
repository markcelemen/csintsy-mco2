% ====================================================================
% FamiLink Knowledge Base
% ====================================================================

% --- DYNAMIC PREDICATES ---
:- dynamic person_male/1.
:- dynamic person_female/1.
:- dynamic has_parent/2.
:- dynamic spouse/2.
:- dynamic explicit_sibling/2.
:- dynamic explicit_uncle/2.
:- dynamic explicit_aunt/2.
:- dynamic explicit_grandparent/2.

% --- UTILITY PREDICATES ---
:- discontiguous logical_error/1.
:- discontiguous has_ancestor/2.
:- discontiguous family_related/2.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% CORE RELATIONSHIP INFERENCE %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

has_child(Parent, Child) :-
    has_parent(Child, Parent).

is_dad(Father, Child) :-
    has_parent(Child, Father),
    person_male(Father).

is_mom(Mother, Child) :-
    has_parent(Child, Mother),
    person_female(Mother).

is_son(Parent, Son) :-
    has_child(Parent, Son),
    person_male(Son).

is_daughter(Parent, Daughter) :-
    has_child(Parent, Daughter),
    person_female(Daughter).

% --- Sibling Logic ---
are_siblings(Person1, Person2) :-
    has_parent(Person1, Parent),
    has_parent(Person2, Parent),
    Person1 \= Person2.

are_siblings(Person1, Person2) :-
    explicit_sibling(Person1, Person2),
    Person1 \= Person2.

is_brother(Brother, Person) :-
    are_siblings(Brother, Person),
    person_male(Brother).

is_sister(Sister, Person) :-
    are_siblings(Sister, Person),
    person_female(Sister).

% --- Spouse Logic ---
is_husband(Husband, Spouse) :-
    is_spouse(Husband, Spouse),
    male(Husband).

is_wife(Wife, Spouse) :-
    is_spouse(Wife, Spouse),
    female(Wife).

is_spouse(Person1, Person2) :-
    spouse(Person1, Person2).


% --- Grandparent, Aunt, Uncle Logic ---
has_grandparent(Grandchild, Grandparent) :-
    (   explicit_grandparent(Grandparent, Grandchild)
    ;   has_parent(Grandchild, Parent),
        has_parent(Parent, Grandparent)
    ),
    Grandchild \= Grandparent.

is_grandfather(Grandchild, Grandfather) :-
    has_grandparent(Grandchild, Grandfather),
    person_male(Grandfather).

is_grandmother(Grandchild, Grandmother) :-
    has_grandparent(Grandchild, Grandmother),
    person_female(Grandmother).

is_uncle(NephewNiece, Uncle) :-
    person_male(Uncle),
    NephewNiece \= Uncle,
    (   explicit_uncle(Uncle, NephewNiece)
    ;   has_parent(NephewNiece, Parent), are_siblings(Parent, Uncle)
    ;   has_grandparent(NephewNiece, Grandparent), has_child(Grandparent, Uncle), \+ has_parent(NephewNiece, Uncle)
    ).

is_aunt(NephewNiece, Aunt) :-
    person_female(Aunt),
    NephewNiece \= Aunt,
    (   explicit_aunt(Aunt, NephewNiece)
    ;   has_parent(NephewNiece, Parent), are_siblings(Parent, Aunt)
    ;   has_grandparent(NephewNiece, Grandparent), has_child(Grandparent, Aunt), \+ has_parent(NephewNiece, Aunt)
    ).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% EXTENDED RELATIONSHIPS      %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

one_step_ancestor(Desc, Anc) :- has_parent(Desc, Anc).
one_step_ancestor(Desc, Anc) :- has_grandparent(Desc, Anc).

has_ancestor(Desc, Anc) :- one_step_ancestor(Desc, Anc).
has_ancestor(Desc, Anc) :-
    one_step_ancestor(Desc, Intermediate),
    has_ancestor(Intermediate, Anc).

is_great_grandparent(GreatGrandchild, GreatGrandparent) :-
    has_parent(GreatGrandchild, Parent),
    has_grandparent(Parent, GreatGrandparent).

are_cousins(Cousin1, Cousin2) :-
    has_parent(Cousin1, Parent1),
    has_parent(Cousin2, Parent2),
    are_siblings(Parent1, Parent2),
    Cousin1 \= Cousin2.

family_related(PersonA, PersonB) :- has_ancestor(PersonA, PersonB).
family_related(PersonA, PersonB) :- has_ancestor(PersonB, PersonA).
family_related(PersonA, PersonB) :- are_siblings(PersonA, PersonB).
family_related(PersonA, PersonB) :- are_cousins(PersonA, PersonB).
family_related(PersonA, PersonB) :-
    has_ancestor(PersonA, Common),
    has_ancestor(PersonB, Common).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% LOGICAL CONSTRAINT CHECKING %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

logical_error('a person cannot be their own parent') :-
    has_parent(X, X), !.

logical_error('a person cannot be both male and female') :-
    person_male(X), person_female(X), !.

logical_error('a person cannot be their own explicit relative (e.g., their own uncle)') :-
    (   explicit_sibling(X, X)
    ;   explicit_uncle(X, X)
    ;   explicit_aunt(X, X)
    ;   explicit_grandparent(X, X)
    ), !.

logical_error('a person cannot be both a parent and grandparent to the same person') :-
    has_parent(Child, Person),
    has_grandparent(Child, Person),
    !.

logical_error('there is a circular ancestor relationship') :-
    has_ancestor(X, Y), has_ancestor(Y, X), !.

logical_error('a person cannot have more than two parents') :-
    has_parent(Child, _),
    findall(P, has_parent(Child, P), Parents),
    sort(Parents, UniqueParents),
    length(UniqueParents, Count),
    Count > 2, !.

logical_error('a person cannot have more than one father') :-
    is_dad(Father1, Child),
    is_dad(Father2, Child),
    Father1 \= Father2, !.

logical_error('a person cannot have more than one mother') :-
    is_mom(Mother1, Child),
    is_mom(Mother2, Child),
    Mother1 \= Mother2, !.

logical_error('close relatives cannot have children together') :-
    has_parent(Child, Parent1),
    has_parent(Child, Parent2),
    Parent1 \= Parent2,
    family_related(Parent1, Parent2), !.

logical_error('a person cannot be a sibling of their ancestor or descendant') :-
    are_siblings(A, B),
    (has_ancestor(A, B) ; has_ancestor(B, A)),
    !.

logical_error('siblings with four known parents must share at least one parent') :-
    are_siblings(Person1, Person2),
    findall(P1, has_parent(Person1, P1), Parents1),
    sort(Parents1, UniqueParents1),
    length(UniqueParents1, 2),
    findall(P2, has_parent(Person2, P2), Parents2),
    sort(Parents2, UniqueParents2),
    length(UniqueParents2, 2),
    intersection(UniqueParents1, UniqueParents2, Common),
    Common == [],
    !.
