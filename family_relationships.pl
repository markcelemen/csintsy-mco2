% ====================================================================
% FAMILY RELATIONSHIP RULES (Knowledge Base)
% ====================================================================

% Dynamic predicates for family facts
:- dynamic person_male/1.
:- dynamic person_female/1.
:- dynamic has_parent/2.
:- dynamic explicit_sibling/2.
:- dynamic explicit_uncle/2.
:- dynamic explicit_aunt/2.
:- dynamic explicit_grandparent/2.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% CORE FAMILY RELATIONSHIPS  %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Basic parent-child relationships
has_child(ParentName, ChildName) :- 
    has_parent(ChildName, ParentName),
    ParentName \= ChildName.

% Gender-specific parent relationships
is_dad(FatherName, ChildName) :- 
    has_parent(ChildName, FatherName),
    person_male(FatherName).

is_mom(MotherName, ChildName) :- 
    has_parent(ChildName, MotherName),
    person_female(MotherName).

% Gender-specific child relationships
is_male_child(SonName, ParentName) :- 
    has_parent(SonName, ParentName),
    person_male(SonName).

is_female_child(DaughterName, ParentName) :- 
    has_parent(DaughterName, ParentName),
    person_female(DaughterName).

% Sibling detection through shared parents
share_parent(Person1, Person2) :- 
    has_parent(Person1, SharedParent),
    has_parent(Person2, SharedParent),
    Person1 \= Person2.

% Explicit and inferred siblings
are_siblings(Person1, Person2) :- 
    explicit_sibling(Person1, Person2),
    Person1 \= Person2.

are_siblings(Person1, Person2) :- 
    share_parent(Person1, Person2),
    Person1 \= Person2.

% Gender-specific siblings
is_male_sibling(BrotherName, SiblingName) :- 
    are_siblings(BrotherName, SiblingName),
    person_male(BrotherName),
    BrotherName \= SiblingName.

is_female_sibling(SisterName, SiblingName) :- 
    are_siblings(SisterName, SiblingName),
    person_female(SisterName),
    SisterName \= SiblingName.

% Grandparent relationships through two generations
has_grandchild(GrandparentName, GrandchildName) :- 
    explicit_grandparent(GrandparentName, GrandchildName),
    GrandparentName \= GrandchildName.

has_grandchild(GrandparentName, GrandchildName) :- 
    has_parent(MiddleGeneration, GrandparentName),
    has_parent(GrandchildName, MiddleGeneration),
    GrandparentName \= GrandchildName,
    GrandparentName \= MiddleGeneration,
    MiddleGeneration \= GrandchildName.

has_grandparent(GrandchildName, GrandparentName) :- 
    has_grandchild(GrandparentName, GrandchildName).

% Gender-specific grandparents
is_grandfather(GrandfatherName, GrandchildName) :- 
    has_grandchild(GrandfatherName, GrandchildName),
    person_male(GrandfatherName).

is_grandmother(GrandmotherName, GrandchildName) :- 
    has_grandchild(GrandmotherName, GrandchildName),
    person_female(GrandmotherName).

% Uncle and Aunt relationships
is_uncle_of(UncleName, NephewNieceName) :- 
    explicit_uncle(UncleName, NephewNieceName),
    person_male(UncleName),
    UncleName \= NephewNieceName.

is_uncle_of(UncleName, NephewNieceName) :- 
    are_siblings(UncleName, ParentName),
    has_parent(NephewNieceName, ParentName),
    person_male(UncleName),
    UncleName \= ParentName,
    UncleName \= NephewNieceName.

is_aunt_of(AuntName, NephewNieceName) :- 
    explicit_aunt(AuntName, NephewNieceName),
    person_female(AuntName),
    AuntName \= NephewNieceName.

is_aunt_of(AuntName, NephewNieceName) :- 
    are_siblings(AuntName, ParentName),
    has_parent(NephewNieceName, ParentName),
    person_female(AuntName),
    AuntName \= ParentName,
    AuntName \= NephewNieceName.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% EXTENDED FAMILY RELATIONS   %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Ancestor relationships (includes parents, grandparents, great-grandparents, etc.)
has_ancestor(Descendant, Ancestor) :- 
    has_parent(Descendant, Ancestor),
    Descendant \= Ancestor.

has_ancestor(Descendant, Ancestor) :- 
    has_parent(Descendant, MiddlePerson),
    has_ancestor(MiddlePerson, Ancestor),
    Descendant \= Ancestor,
    Descendant \= MiddlePerson,
    MiddlePerson \= Ancestor.

% Direct implementation of grandparents as ancestors
has_ancestor(Descendant, GrandAncestor) :-
    has_grandparent(Descendant, GrandAncestor),
    Descendant \= GrandAncestor.

% Great-grandparent relationships as ancestors
has_ancestor(Descendant, GreatGrandAncestor) :-
    has_parent(Descendant, Parent),
    has_grandparent(Parent, GreatGrandAncestor),
    Descendant \= GreatGrandAncestor,
    Descendant \= Parent,
    Parent \= GreatGrandAncestor.

% Descendant relationships (reverse of ancestor)
has_descendant(Ancestor, Descendant) :- 
    has_ancestor(Descendant, Ancestor).

% Great-grandparent relationships
is_great_grandparent(GreatGrandparent, GreatGrandchild) :-
    has_parent(GreatGrandchild, Parent),
    has_grandparent(Parent, GreatGrandparent),
    GreatGrandparent \= GreatGrandchild,
    GreatGrandparent \= Parent,
    Parent \= GreatGrandchild.

% Cousin relationships (children of siblings)
are_cousins(Cousin1, Cousin2) :-
    has_parent(Cousin1, Parent1),
    has_parent(Cousin2, Parent2),
    are_siblings(Parent1, Parent2),
    Cousin1 \= Cousin2,
    Parent1 \= Parent2.

% First cousins once removed (child of cousin or cousin of parent)
are_cousins_once_removed(Person1, Person2) :-
    has_parent(Person1, Parent),
    are_cousins(Parent, Person2).

are_cousins_once_removed(Person1, Person2) :-
    has_parent(Person2, Parent),
    are_cousins(Person1, Parent).

% Second cousins (children of first cousins)
are_second_cousins(Cousin1, Cousin2) :-
    has_parent(Cousin1, Parent1),
    has_parent(Cousin2, Parent2),
    are_cousins(Parent1, Parent2),
    Cousin1 \= Cousin2.

% Common ancestor detection
common_ancestor(PersonA, PersonB, Ancestor) :-
    has_ancestor(PersonA, Ancestor),
    has_ancestor(PersonB, Ancestor),
    PersonA \= PersonB,
    PersonA \= Ancestor,
    PersonB \= Ancestor.

% Relative detection
% This includes ALL blood relations according to genealogical standards
family_related(PersonA, PersonB) :- 
    PersonA \= PersonB,
    (   % Direct parent-child relationships
        has_parent(PersonA, PersonB)
    ;   has_parent(PersonB, PersonA)
    ;   % Sibling relationships
        are_siblings(PersonA, PersonB)
    ;   % Grandparent-grandchild relationships
        has_grandparent(PersonA, PersonB)
    ;   has_grandparent(PersonB, PersonA)
    ;   % Great-grandparent relationships
        is_great_grandparent(PersonA, PersonB)
    ;   is_great_grandparent(PersonB, PersonA)
    ;   % Uncle/Aunt - Nephew/Niece relationships
        is_uncle_of(PersonA, PersonB)
    ;   is_aunt_of(PersonA, PersonB)
    ;   is_uncle_of(PersonB, PersonA)
    ;   is_aunt_of(PersonB, PersonA)
    ;   % Cousin relationships
        are_cousins(PersonA, PersonB)
    ;   are_cousins_once_removed(PersonA, PersonB)
    ;   are_second_cousins(PersonA, PersonB)
    ;   % Any ancestor-descendant relationship (covers all generations)
        has_ancestor(PersonA, PersonB)
    ;   has_ancestor(PersonB, PersonA)
    ;   % Extended relations through common ancestors (covers distant cousins, etc.)
        common_ancestor(PersonA, PersonB, _)
    ;   % Direct definition for second cousins
        (   has_parent(PersonA, ParentA),
            has_parent(PersonB, ParentB),
            has_parent(ParentA, GrandparentA),
            has_parent(ParentB, GrandparentB),
            are_siblings(GrandparentA, GrandparentB)
        )
    ).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% LOGICAL CONSTRAINT CHECKING %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Self-relationship impossibilities
logical_error(self_relation) :- has_parent(X, X).
logical_error(self_relation) :- explicit_sibling(X, X).
logical_error(self_relation) :- explicit_grandparent(X, X).
logical_error(self_relation) :- explicit_uncle(X, X).
logical_error(self_relation) :- explicit_aunt(X, X).

% Circular relationship detection
logical_error(circular_family) :- 
    has_parent(X, Y), 
    has_parent(Y, X).

logical_error(three_gen_loop) :- 
    has_parent(X, Y), 
    has_parent(Y, Z), 
    has_parent(Z, X).

% Four generation loop detection
logical_error(four_gen_loop) :-
    has_parent(X, Y),
    has_parent(Y, Z),
    has_parent(Z, W),
    has_parent(W, X).

% General ancestor loop detection
logical_error(ancestor_loop) :-
    has_ancestor(X, Y),
    has_ancestor(Y, X).

% Gender contradiction detection
logical_error(gender_mismatch) :- 
    person_male(X), 
    person_female(X).

logical_error(invalid_father_gender) :- 
    is_dad(X, _), 
    person_female(X).

logical_error(invalid_mother_gender) :- 
    is_mom(X, _), 
    person_male(X).

logical_error(invalid_son_gender) :- 
    is_male_child(X, _), 
    person_female(X).

logical_error(invalid_daughter_gender) :- 
    is_female_child(X, _), 
    person_male(X).

logical_error(invalid_brother_gender) :- 
    is_male_sibling(X, _), 
    person_female(X).

logical_error(invalid_sister_gender) :- 
    is_female_sibling(X, _), 
    person_male(X).

logical_error(invalid_grandfather_gender) :- 
    is_grandfather(X, _), 
    person_female(X).

logical_error(invalid_grandmother_gender) :- 
    is_grandmother(X, _), 
    person_male(X).

logical_error(invalid_uncle_gender) :- 
    is_uncle_of(X, _), 
    person_female(X).

logical_error(invalid_aunt_gender) :- 
    is_aunt_of(X, _), 
    person_male(X).

% Biological family constraints
logical_error(too_many_biological_parents) :- 
    has_parent(Child, Parent1), 
    has_parent(Child, Parent2), 
    has_parent(Child, Parent3),
    Parent1 \= Parent2, 
    Parent2 \= Parent3, 
    Parent1 \= Parent3.

logical_error(multiple_biological_fathers) :-
    is_dad(Father1, Child),
    is_dad(Father2, Child),
    Father1 \= Father2.

logical_error(multiple_biological_mothers) :-
    is_mom(Mother1, Child),
    is_mom(Mother2, Child),
    Mother1 \= Mother2.

% Relationship hierarchy impossibilities
logical_error(sibling_parent_impossible) :- 
    are_siblings(X, Y), 
    has_parent(Y, X).

logical_error(parent_grandparent_impossible) :- 
    has_parent(X, Y), 
    has_grandchild(X, Y).

logical_error(uncle_parent_impossible) :- 
    is_uncle_of(X, Y), 
    has_parent(Y, X).

logical_error(aunt_parent_impossible) :- 
    is_aunt_of(X, Y), 
    has_parent(Y, X).

logical_error(sibling_grandparent_impossible) :-
    are_siblings(X, Y),
    has_grandchild(X, Y).

logical_error(sibling_grandparent_impossible) :-
    are_siblings(X, Y),
    has_grandchild(Y, X).

% Additional biological impossibilities
logical_error(person_their_own_ancestor) :-
    has_ancestor(X, X).

logical_error(person_their_own_descendant) :-
    has_descendant(X, X).

% Age-related impossibilities
logical_error(cousin_grandparent_impossible) :-
    are_cousins(X, Y),
    has_grandchild(X, Y).

logical_error(cousin_grandparent_impossible) :-
    are_cousins(X, Y),
    has_grandchild(Y, X).

% Relative parents constraint
% This prevents relatives from having a child together
logical_error(relatives_having_child) :-
    has_parent(Child, Parent1),
    has_parent(Child, Parent2),
    Parent1 \= Parent2,
    family_related(Parent1, Parent2).

% Special cases of particularly close relatives that should never have children
logical_error(siblings_having_child) :-
    has_parent(Child, Parent1),
    has_parent(Child, Parent2),
    Parent1 \= Parent2,
    are_siblings(Parent1, Parent2).

logical_error(parent_child_having_child) :-
    has_parent(Child, Parent1),
    has_parent(Child, Parent2),
    Parent1 \= Parent2,
    has_parent(Parent1, Parent2).

logical_error(parent_child_having_child) :-
    has_parent(Child, Parent1),
    has_parent(Child, Parent2),
    Parent1 \= Parent2,
    has_parent(Parent2, Parent1).
