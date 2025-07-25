from pyswip import Prolog
import re

def initialize_prolog():
    prolog = Prolog()
    try:
        prolog.consult("relationships.pl")
    except Exception as e:
        return None, f"Error loading Prolog file: {str(e)}"
    return prolog, "Prolog initialized successfully."

def add_fact(prolog, fact):
    try:
        prolog.assertz(fact)

        # Check for contradictions
        contradictions = list(prolog.query("contradiction(Reason)"))
        # TODO: add print statement for contradictions found here
        if contradictions:
            # If a contradiction is found, then remove the fact
            prolog.retract(fact)
            return False
        return True,
    except Exception as e:
        return f"Error: {str(e)}"

def check_statement(prolog, statement):
    statement = statement.strip()
    statement = statement.lower()

    # Statement 1: X and Y are siblings.
    # NOTE: Sibling means at least one parent must be the same
    sibling_match = re.match(r"([a-z]+) and ([a-z]+) are siblings\.", statement)

    if sibling_match:
        # Evaluate the input
        sib_1, sib_2 = sibling_match.groups()
        print(sib_1, sib_2)

        # Add the facts
        add_sib1 = add_fact(prolog, f"sibling('{sib_1}', '{sib_2}')")
        add_sib2 = add_fact(prolog, f"sibling('{sib_2}', '{sib_1}')")

        # Return the correct output
        if add_sib1 and add_sib2:
            return "OK! I learned something."
        else:
            return "Oops! Something went wrong."

    # Statement 2: X is a brother of Y.
    brother_match = re.match(r"([a-z]+) is a brother of ([a-z]+)\.", statement)

    if brother_match:
        # Evaluate the input
        brother, sibling = brother_match.groups()
        print(brother, sibling)

        # Add the facts
        add_sib1 = add_fact(prolog, f"sibling('{brother}', '{sibling}')")
        add_sib2 = add_fact(prolog, f"sibling('{sibling}', '{brother}')")
        add_sex = add_fact(prolog, f"male('{brother}')")

        # Return the correct output
        if add_sib1 and add_sib2 and add_sex:
            return "OK! I learned something."
        else:
            return "Oops! Something went wrong."

    # Statement 3: X is a sister of Y.
    sister_match = re.match(r"([a-z]+) is a sister of ([a-z]+)\.", statement)

    if sister_match:
        # Evaluate the input
        sister, sibling = sister_match.groups()
        print(sister, sibling)

        # Add the facts
        add_sib1 = add_fact(prolog, f"sibling('{sister}', '{sibling}')")
        add_sib2 = add_fact(prolog, f"sibling('{sibling}', '{sister}')")
        add_sex = add_fact(prolog, f"female('{sister}')")

        # Return the correct output
        if add_sib1 and add_sib2 and add_sex:
            return "OK! I learned something."
        else:
            return "Oops! Something went wrong."

    # Statement 4: X is the father of Y.
    father_match = re.match(r"([a-z]+) is a father of ([a-z]+)\.", statement)

    if father_match:
        # Evaluate the input
        father, child = father_match.groups()
        print(father, child)

        # Add the facts
        add_parent = add_fact(prolog, f"parent('{father}', '{child}')")
        add_child = add_fact(prolog, f"child('{child}', '{father}')")
        add_sex = add_fact(prolog, f"male('{father}')")

        # Return the correct output
        if add_parent and add_child and add_sex:
            return "OK! I learned something."
        else:
            return "Oops! Something went wrong."

    # Statement 5: X is the mother of Y.
    mother_match = re.match(r"([a-z]+) is a mother of ([a-z]+)\.", statement)

    if mother_match:
        # Evaluate the input
        mother, child = mother_match.groups()
        print(mother, child)

        # Add the facts
        add_parent = add_fact(prolog, f"parent('{mother}', '{child}')")
        add_child = add_fact(prolog, f"child('{child}', '{mother}')")
        add_sex = add_fact(prolog, f"female('{mother}')")

        # Return correct output
        if add_parent and add_child and add_sex:
            return "OK! I learned something."
        else:
            return "Oops! Something went wrong."

    # Statement 6: X and Y are the parents of Z.
    parents_match = re.match(r"([a-z]+) and ([a-z]+) are the parents of ([a-z]+)\.", statement)

    if parents_match:
        # Evaluate the input
        parent1, parent2, child = parents_match.groups()
        print(parent1, parent2, child)

        # Add the facts
        add_parent1 = add_fact(prolog, f"parent('{parent1}', '{child}')")
        add_parent2 = add_fact(prolog, f"parent('{parent2}', '{child}')")
        add_child1 = add_fact(prolog, f"child('{child}', '{parent1}')")
        add_child2 = add_fact(prolog, f"child('{child}', '{parent2}')")

        # Return the correct output
        if add_parent1 and add_parent2 and add_child1 and add_child2:
            return "OK! I learned something."
        else:
            return "Oops! Something went wrong."

    # Statement 7: X is a grandfather of Y.
    grandfather_match = re.match(r"([a-z]+) is a grandfather of ([a-z]+)\.", statement)

    if grandfather_match:
        # Evaluate the input
        grandfather, grandchild = grandfather_match.groups()
        print(grandfather, grandchild)

        # Add the facts
        add_grandparent = add_fact(prolog, f"grandparent('{grandfather}', '{grandchild}')")
        add_grandchild = add_fact(prolog, f"grandchild('{grandchild}', '{grandfather}')")
        add_sex = add_fact(prolog, f"male('{grandfather}')")

        # Return the correct output
        if add_grandparent and add_grandchild and add_sex:
            return "OK! I learned something."
        else:
            return "Oops! Something went wrong."

    # Statement 8: X is a grandmother of Y.
    grandmother_match = re.match(r"([a-z]+) is a grandfather of ([a-z]+)\.", statement)

    if grandmother_match:
        # Evaluate the input
        grandmother, grandchild = grandmother_match.groups()
        print(grandmother, grandchild)

        # Add the facts
        add_grandparent = add_fact(prolog, f"grandparent('{grandmother}', '{grandchild}')")
        add_grandchild = add_fact(prolog, f"grandchild('{grandchild}', '{grandmother}')")
        add_sex = add_fact(prolog, f"female('{grandmother}')")

        # Return the correct output
        if add_grandparent and add_grandchild and add_sex:
            return "OK! I learned something."
        else:
            return "Oops! Something went wrong."

    # Statement 9: X is a child of Y.
    child_match = re.match(r"([a-z]+) is a child of ([a-z]+)\.", statement)

    if child_match:
        # Evaluate the input
        child, parent = child_match.groups()
        print(child, parent)

        # Add the facts
        add_child = add_fact(prolog, f"child('{child}', '{parent}')")
        add_parent = add_fact(prolog, f"parent('{parent}', '{child}')")

        # Return the correct output
        if add_child and add_parent:
            return "OK! I learned something."
        else:
            return "Oops! Something went wrong."
    
    # Statement 10: W, X, and Y are children of Z.
    # TODO: edit because this can be two or more children
    children_match = re.match(r"([a-z]+), ([a-z]+), and ([a-z]+) are children of ([a-z]+)\.", statement)

    if children_match:
        # Evaluate the input
        child1, child2, child3, parent = children_match.groups()
        print(child1, child2, child3, parent)

        # Add the facts
        add_child1 = add_fact(prolog, f"child('{child1}', '{parent}')")
        add_child2 = add_fact(prolog, f"child('{child2}', '{parent}')")
        add_child3 = add_fact(prolog, f"child('{child3}', '{parent}')")
        add_parent1 = add_fact(prolog, f"parent('{parent}', '{child1}')")
        add_parent2 = add_fact(prolog, f"parent('{parent}', '{child2}')")
        add_parent3 = add_fact(prolog, f"parent('{parent}', '{child3}')")

        # Return the correct output
        if add_child1 and add_child2 and add_child3 and add_parent1 and add_parent2 and add_parent3:
            return "OK! I learned something."
        else:
            return "Oops! Something went wrong."

    # Statement 11: X is a son of Y.
    son_match = re.match(r"([a-z]+) is a son of ([a-z]+)\.", statement)

    if son_match:
        # Evaluate the input
        son, parent = son_match.groups()
        print(son, parent)

        # Add the facts
        add_child = add_fact(prolog, f"child('{son}', '{parent}')")
        add_sex = add_fact(prolog, f"male('{son}')")
        add_parent = add_fact(prolog, f"parent('{parent}', '{son}')")

        # Return the correct output
        if add_child and add_sex and add_parent:
            return "OK! I learned something."
        else:
            return "Oops! Something went wrong."

    # Statement 12: X is a daughter of Y.
    daughter_match = re.match(r"([a-z]+) is a daughter of ([a-z]+)\.", statement)

    if daughter_match:
        # Evaluate the input
        daughter, parent = daughter_match.groups()
        print(daughter, parent)

        # Add the facts
        add_child = add_fact(prolog, f"child('{daughter}', '{parent}')")
        add_sex = add_fact(prolog, f"female('{daughter}')")
        add_parent = add_fact(prolog, f"parent('{parent}', '{daughter}')")

        # Return the correct output
        if add_child and add_sex and add_parent:
            return "OK! I learned something."
        else:
            return "Oops! Something went wrong."

    # Statement 13: X is an uncle of Y.
    # NOTE: nibling means nephew or niece 
    uncle_match = re.match(r"([a-z]+) is an uncle of ([a-z]+)\.", statement)

    if uncle_match:
        # Evaluate the input
        uncle, nibling =  uncle_match.groups()
        print(uncle, nibling)

        # Add the facts
        add_uncle = add_fact(prolog, f"uncle('{uncle}', '{nibling}')")
        add_sex = add_fact(prolog, f"male('{uncle}')")

        # Return the correct output
        if add_uncle and add_sex:
            return "OK! I learned something."
        else:
            return "Oops! Something went wrong."

    # Statement 14: X is an aunt of Y.
    aunt_match = re.match(r"([a-z]+) is an aunt of ([a-z]+)\.", statement)

    if aunt_match:
        # Evaluate the input
        aunt, nibling =  aunt_match.groups()
        print(aunt, nibling)

        # Add the facts
        add_aunt = add_fact(prolog, f"aunt('{aunt}', '{nibling}')")
        add_sex = add_fact(prolog, f"male('{aunt}')")

        # Return the correct output
        if add_aunt and add_sex:
            return "OK! I learned something."
        else:
            return "Oops! Something went wrong."

    return "Oops! I don't know that statement format."