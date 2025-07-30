#!/usr/bin/env python3
"""
FamiLink Family Relationship Chatbot
------------------------------------
This module contains the core logic for the FamiLink chatbot. It uses a
Prolog back-end for reasoning and constraint checking, and provides a
text-based interface for user interaction.
"""

import os
import re
from pyswip import Prolog
from typing import List, Dict, Set

class FamilyRelationshipBot:
    """
    Main chatbot class for processing family relationship statements and queries.
    """

    def __init__(self, knowledge_file: str = "family_relationships.pl"):
        """
        Initializes the chatbot, loading the Prolog knowledge base.
        It performs an aggressive pre-cleaning to ensure a fresh state.
        """
        self.prolog_engine = Prolog()
        
        # Aggressively retract all known predicates before loading the file
        # to prevent state-carryover issues between sessions or reloads.
        known_predicates = [
            "person_male/1", "person_female/1", "has_parent/2", "has_spouse/2",
            "explicit_sibling/2", "explicit_uncle/2", "explicit_aunt/2",
            "explicit_grandparent/2"
        ]
        for pred in known_predicates:
            name, arity_str = pred.split('/')
            arity = int(arity_str)
            try:
                # Create a query like 'retractall(person_male(_))'
                list(self.prolog_engine.query(f"retractall({name}({','.join(['_'] * arity)}))"))
            except Exception:
                # Ignore errors if a predicate doesn't exist to be retracted
                continue
        
        # Construct absolute path to the knowledge base file
        script_dir = os.path.dirname(os.path.abspath(__file__))
        self.absolute_knowledge_path = os.path.join(script_dir, knowledge_file)

        if not os.path.exists(self.absolute_knowledge_path):
            raise FileNotFoundError(f"Knowledge base file not found: {self.absolute_knowledge_path}")
        
        self.prolog_engine.consult(self.absolute_knowledge_path)

    def clear_all_facts(self):
        """Removes all dynamic facts from the knowledge base for a clean reset."""
        fact_types = [
            ("person_male", 1), ("person_female", 1), ("has_parent", 2),
            ("has_spouse", 2), ("explicit_sibling", 2), ("explicit_uncle", 2),
            ("explicit_aunt", 2), ("explicit_grandparent", 2)
        ]
        for fact_type, arity in fact_types:
            try:
                list(self.prolog_engine.query(f"retractall({fact_type}({','.join(['_'] * arity)}))"))
            except Exception:
                continue

    def execute_user_input(self, user_input: str) -> str:
        """Main method to process user input and return an appropriate response."""
        user_input = user_input.strip()
        if not user_input:
            return "Please enter a family relationship statement or question."

        # Handle system commands
        commands = {
            "exit": "exit_command", "quit": "exit_command", "goodbye": "exit_command",
            "help": self.display_help_information,
            "status": self.show_knowledge_base_summary,
            "reset": self._execute_reset_command,
            "clear": "clear_screen", "cls": "clear_screen"
        }
        
        handler = commands.get(user_input.lower())
        if handler:
            return handler() if callable(handler) else handler

        # Differentiate between questions and statements
        if user_input.endswith("?"):
            return self.process_family_question(user_input)
        else:
            return self.process_family_statement(user_input)

    def _execute_reset_command(self) -> str:
        """Helper function for the 'reset' command."""
        self.clear_all_facts()
        return "All family relationship data has been cleared."

    def process_family_statement(self, statement: str) -> str:
        """Parses and processes factual statements about family relationships."""
        statement = statement.rstrip(".")
        
        statement_handlers = [
            (r"(\w+) is the father of (\w+)", self.handle_father_statement),
            (r"(\w+) is the mother of (\w+)", self.handle_mother_statement),
            (r"(\w+) and (\w+) are the parents of (\w+)", self.handle_parents_statement),
            (r"(\w+) is a child of (\w+)", self.handle_child_statement),
            (r"(\w+) is a son of (\w+)", self.handle_son_statement),
            (r"(\w+) is a daughter of (\w+)", self.handle_daughter_statement),
            (r"(\w+) and (\w+) are children of (\w+)", self.handle_two_children_statement),
            (r"(\w+), (\w+), and (\w+) are children of (\w+)", self.handle_three_children_statement),
            (r"(\w+), (\w+), (\w+), and (\w+) are children of (\w+)", self.handle_four_children_statement),
            (r"(\w+) and (\w+) are siblings", self.handle_siblings_statement),
            (r"(\w+) is a brother of (\w+)", self.handle_brother_statement),
            (r"(\w+) is a sister of (\w+)", self.handle_sister_statement),
            (r"(\w+) is a grandfather of (\w+)", self.handle_grandfather_statement),
            (r"(\w+) is a grandmother of (\w+)", self.handle_grandmother_statement),
            (r"(\w+) is an uncle of (\w+)", self.handle_uncle_statement),
            (r"(\w+) is an aunt of (\w+)", self.handle_aunt_statement),
            (r"(\w+) is the spouse of (\w+)", self.handle_spouse_statement),
        ]

        for pattern, handler_method in statement_handlers:
            if match := re.fullmatch(pattern, statement, re.IGNORECASE):
                names = [self.standardize_name(name) for name in match.groups()]
                return handler_method(*names)
        
        return "I don't understand that statement format. Please check the help for valid patterns."

    def process_family_question(self, question: str) -> str:
        """Parses and processes questions about family relationships."""
        question = question.rstrip("?")
        
        question_handlers = [
            # Yes/No Questions
            (r"is (\w+) the father of (\w+)", self._handle_q_is_father),
            (r"is (\w+) the mother of (\w+)", self._handle_q_is_mother),
            (r"are (\w+) and (\w+) the parents of (\w+)", self._handle_q_are_parents),
            (r"is (\w+) a child of (\w+)", self._handle_q_is_child),
            (r"are (\w+) and (\w+) children of (\w+)", self._handle_q_are_children),
            (r"is (\w+) a son of (\w+)", self._handle_q_is_son),
            (r"is (\w+) a daughter of (\w+)", self._handle_q_is_daughter),
            (r"are (\w+) and (\w+) siblings", self._handle_q_are_siblings),
            (r"is (\w+) a brother of (\w+)", self._handle_q_is_brother),
            (r"is (\w+) a sister of (\w+)", self._handle_q_is_sister),
            (r"is (\w+) a grandfather of (\w+)", self._handle_q_is_grandfather),
            (r"is (\w+) a grandmother of (\w+)", self._handle_q_is_grandmother),
            (r"is (\w+) an uncle of (\w+)", self._handle_q_is_uncle),
            (r"is (\w+) an aunt of (\w+)", self._handle_q_is_aunt),
            (r"are (\w+) and (\w+) cousins", self._handle_q_are_cousins),
            (r"are (\w+) and (\w+) relatives", self._handle_q_are_relatives),
            (r"is (\w+) the spouse of (\w+)", self._handle_q_is_spouse),
            # "Who" Questions
            (r"who is the father of (\w+)", self._handle_q_who_father),
            (r"who is the mother of (\w+)", self._handle_q_who_mother),
            (r"who are the parents of (\w+)", self._handle_q_who_parents),
            (r"who are the children of (\w+)", self._handle_q_who_children),
            (r"who are the siblings of (\w+)", self._handle_q_who_siblings),
            (r"who are the brothers of (\w+)", self._handle_q_who_brothers),
            (r"who are the sisters of (\w+)", self._handle_q_who_sisters),
            (r"who are the daughters of (\w+)", self._handle_q_who_daughters),
            (r"who are the sons of (\w+)", self._handle_q_who_sons),
            (r"who is the spouse of (\w+)", self._handle_q_who_spouse),
        ]

        for pattern, handler_method in question_handlers:
            if match := re.fullmatch(pattern, question, re.IGNORECASE):
                names = [self.standardize_name(name) for name in match.groups()]
                return handler_method(*names)
        
        return "I don't understand that question format. Please check the help for valid patterns."

    def safely_add_facts(self, fact_list: List[str]) -> bool:
        """
        Adds facts transactionally. If any added fact leads to a logical
        contradiction, all facts from this transaction are retracted.
        """
        added_facts = []
        try:
            for fact in fact_list:
                self.prolog_engine.asserta(fact)
                added_facts.append(fact)
            
            # Check for any logical errors defined in the Prolog file
            if list(self.prolog_engine.query("logical_error(_)")):
                raise ValueError("Logical contradiction detected by Prolog.")
            
            return True
        except Exception:
            # If an error occurs, roll back the changes
            for fact in added_facts:
                self.prolog_engine.retract(fact)
            return False

    def standardize_name(self, name: str) -> str:
        """Converts name to a robust Prolog atom format (lowercase)."""
        return name.strip().lower()

    def _ask_prolog_bool(self, query: str) -> str:
        """Asks a yes/no question to Prolog and returns 'Yes' or 'No'."""
        return "Yes" if list(self.prolog_engine.query(query)) else "No"

    def _find_prolog_all(self, query_template: str) -> str:
        """Finds all solutions for a variable 'X' in a Prolog query."""
        query = f"findall(X, {query_template}, People)"
        try:
            solutions = list(self.prolog_engine.query(query))
            if solutions and solutions[0]['People']:
                names = [str(name).capitalize() for name in solutions[0]['People']]
                return self._format_list_of_names(names)
            return "No one"
        except Exception:
            return "I couldn't figure that out."

    # --- Statement Handler Methods (for Asserting Facts) ---

    def _generic_handler(self, facts: List[str]) -> str:
        """A generic handler for adding facts and returning a standard response."""
        return "OK! I learned something." if self.safely_add_facts(facts) else "That's impossible!"

    def handle_father_statement(self, f: str, c: str) -> str:
        return self._generic_handler([f"has_parent({c},{f})", f"person_male({f})"])
    
    def handle_mother_statement(self, m: str, c: str) -> str:
        return self._generic_handler([f"has_parent({c},{m})", f"person_female({m})"])

    def handle_parents_statement(self, p1: str, p2: str, c: str) -> str:
        return self._generic_handler([f"has_parent({c},{p1})", f"has_parent({c},{p2})"])

    def handle_child_statement(self, c: str, p: str) -> str:
        return self._generic_handler([f"has_parent({c},{p})"])

    def handle_son_statement(self, s: str, p: str) -> str:
        return self._generic_handler([f"has_parent({s},{p})", f"person_male({s})"])

    def handle_daughter_statement(self, d: str, p: str) -> str:
        return self._generic_handler([f"has_parent({d},{p})", f"person_female({d})"])

    def handle_two_children_statement(self, c1: str, c2: str, p: str) -> str:
        return self._generic_handler([f"has_parent({c1},{p})", f"has_parent({c2},{p})"])

    def handle_three_children_statement(self, c1: str, c2: str, c3: str, p: str) -> str:
        return self._generic_handler([f"has_parent({c1},{p})", f"has_parent({c2},{p})", f"has_parent({c3},{p})"])

    def handle_four_children_statement(self, c1: str, c2: str, c3: str, c4: str, p: str) -> str:
        return self._generic_handler([f"has_parent({c1},{p})", f"has_parent({c2},{p})", f"has_parent({c3},{p})", f"has_parent({c4},{p})"])

    def handle_siblings_statement(self, s1: str, s2: str) -> str:
        return self._generic_handler([f"explicit_sibling({s1},{s2})", f"explicit_sibling({s2},{s1})"])

    def handle_brother_statement(self, b: str, s: str) -> str:
        return self._generic_handler([f"explicit_sibling({b},{s})", f"explicit_sibling({s},{b})", f"person_male({b})"])

    def handle_sister_statement(self, sis: str, s: str) -> str:
        return self._generic_handler([f"explicit_sibling({sis},{s})", f"explicit_sibling({s},{sis})", f"person_female({sis})"])

    def handle_grandfather_statement(self, g: str, c: str) -> str:
        return self._generic_handler([f"explicit_grandparent({g},{c})", f"person_male({g})"])

    def handle_grandmother_statement(self, g: str, c: str) -> str:
        return self._generic_handler([f"explicit_grandparent({g},{c})", f"person_female({g})"])

    def handle_uncle_statement(self, u: str, c: str) -> str:
        return self._generic_handler([f"explicit_uncle({u},{c})", f"person_male({u})"])

    def handle_aunt_statement(self, a: str, c: str) -> str:
        return self._generic_handler([f"explicit_aunt({a},{c})", f"person_female({a})"])

    def handle_spouse_statement(self, p1: str, p2: str) -> str:
        return self._generic_handler([f"has_spouse({p1},{p2})", f"has_spouse({p2},{p1})"])

    # --- Question Handler Methods (for Answering Queries) ---

    def _handle_q_is_father(self, p: str, c: str) -> str: return self._ask_prolog_bool(f"is_dad({p}, {c})")
    def _handle_q_is_mother(self, p: str, c: str) -> str: return self._ask_prolog_bool(f"is_mom({p}, {c})")
    def _handle_q_are_parents(self, p1: str, p2: str, c: str) -> str: return self._ask_prolog_bool(f"(has_parent({c}, {p1}), has_parent({c}, {p2}))")
    def _handle_q_is_child(self, c: str, p: str) -> str: return self._ask_prolog_bool(f"has_child({p}, {c})")
    def _handle_q_are_children(self, c1: str, c2: str, p: str) -> str: return self._ask_prolog_bool(f"(has_child({p}, {c1}), has_child({p}, {c2}))")
    def _handle_q_is_son(self, c: str, p: str) -> str: return self._ask_prolog_bool(f"is_son({c}, {p})")
    def _handle_q_is_daughter(self, c: str, p: str) -> str: return self._ask_prolog_bool(f"is_daughter({c}, {p})")
    def _handle_q_are_siblings(self, p1: str, p2: str) -> str: return self._ask_prolog_bool(f"are_siblings({p1}, {p2})")
    def _handle_q_is_brother(self, p: str, s: str) -> str: return self._ask_prolog_bool(f"is_brother({p}, {s})")
    def _handle_q_is_sister(self, p: str, s: str) -> str: return self._ask_prolog_bool(f"is_sister({p}, {s})")
    def _handle_q_is_grandfather(self, gp: str, gc: str) -> str: return self._ask_prolog_bool(f"is_grandfather({gc}, {gp})")
    def _handle_q_is_grandmother(self, gm: str, gc: str) -> str: return self._ask_prolog_bool(f"is_grandmother({gc}, {gm})")
    def _handle_q_is_uncle(self, u: str, n: str) -> str: return self._ask_prolog_bool(f"is_uncle({n}, {u})")
    def _handle_q_is_aunt(self, a: str, n: str) -> str: return self._ask_prolog_bool(f"is_aunt({n}, {a})")
    def _handle_q_are_cousins(self, p1: str, p2: str) -> str: return self._ask_prolog_bool(f"are_cousins({p1}, {p2})")
    def _handle_q_are_relatives(self, p1: str, p2: str) -> str: return self._ask_prolog_bool(f"family_related({p1}, {p2})")
    def _handle_q_is_spouse(self, p1: str, p2: str) -> str: return self._ask_prolog_bool(f"has_spouse({p1}, {p2})")

    def _handle_q_who_father(self, c: str) -> str: return self._find_prolog_all(f"is_dad(X, {c})")
    def _handle_q_who_mother(self, c: str) -> str: return self._find_prolog_all(f"is_mom(X, {c})")
    def _handle_q_who_parents(self, c: str) -> str: return self._find_prolog_all(f"has_parent({c}, X)")
    def _handle_q_who_children(self, p: str) -> str: return self._find_prolog_all(f"has_child({p}, X)")
    def _handle_q_who_siblings(self, p: str) -> str: return self._find_prolog_all(f"are_siblings(X, {p})")
    def _handle_q_who_brothers(self, p: str) -> str: return self._find_prolog_all(f"is_brother(X, {p})")
    def _handle_q_who_sisters(self, p: str) -> str: return self._find_prolog_all(f"is_sister(X, {p})")
    def _handle_q_who_daughters(self, p: str) -> str: return self._find_prolog_all(f"is_daughter(X, {p})")
    def _handle_q_who_sons(self, p: str) -> str: return self._find_prolog_all(f"is_son(X, {p})")
    def _handle_q_who_spouse(self, p: str) -> str: return self._find_prolog_all(f"has_spouse({p}, X)")

    # --- Formatting and Display ---

    def _format_list_of_names(self, names: List[str]) -> str:
        """Formats a list of names into a human-readable string."""
        if not names:
            return "No one"
        
        names = sorted(list(set(names)))
        if len(names) == 1:
            return names[0]
        if len(names) == 2:
            return f"{names[0]} and {names[1]}"
        
        return f"{', '.join(names[:-1])}, and {names[-1]}"

    def show_knowledge_base_summary(self) -> str:
        """Displays a robust, accurate summary of all known facts."""
        try:
            males = {str(m['M']).capitalize() for m in self.prolog_engine.query("person_male(M)")}
            females = {str(f['F']).capitalize() for f in self.prolog_engine.query("person_female(F)")}
            
            parent_map: Dict[str, Set[str]] = {}
            people_in_relations = set()
            for pair in self.prolog_engine.query("has_parent(Child, Parent)"):
                child_name = str(pair['Child']).capitalize()
                parent_name = str(pair['Parent']).capitalize()
                people_in_relations.add(child_name)
                people_in_relations.add(parent_name)
                parent_map.setdefault(child_name, set()).add(parent_name)

            spouse_map: Dict[str, str] = {}
            for pair in self.prolog_engine.query("has_spouse(P1, P2)"):
                p1_name = str(pair['P1']).capitalize()
                p2_name = str(pair['P2']).capitalize()
                people_in_relations.add(p1_name)
                people_in_relations.add(p2_name)
                spouse_map[p1_name] = p2_name

            all_people = sorted(list(males | females | people_in_relations))

            if not all_people:
                return "The knowledge base is empty."

            output = ["="*25, "KNOWLEDGE BASE STATUS", "="*25]
            output.append(f"\nTotal People Known: {len(all_people)}")
            output.append(f"  - Males: {len(males)}")
            output.append(f"  - Females: {len(females)}")
            
            output.append("\n--- Family Structure ---")
            for person in all_people:
                gender = "Male" if person in males else "Female" if person in females else "Gender Unknown"
                parents_list = sorted(list(parent_map.get(person, set())))
                parents_str = self._format_list_of_names(parents_list) if parents_list else "Unknown"
                spouse_str = spouse_map.get(person, "None")
                
                output.append(f"- {person} ({gender})")
                output.append(f"  - Parents: {parents_str}")
                output.append(f"  - Spouse: {spouse_str}")

            return "\n".join(output)
        except Exception as e:
            return f"Error generating status report: {e}"

    def display_help_information(self) -> str:
        """Returns a string containing the help message."""
        return """
FamiLink Help Guide
--------------------
You can tell me facts about a family or ask me questions.
Names must be a single word starting with a capital letter.

1. STATEMENTS (Telling me facts)
   End statements with a period '.' or nothing.

   - Parent-Child:
     "Robert is the father of Stannis."
     "Joanna is the mother of Cersei."
     "Tywin and Joanna are the parents of Jaime."
     "Podrick is a child of Tyrion."
     "Gendry is a son of Robert."
     "Myrcella is a daughter of Cersei."
     "Tommen and Myrcella are children of Robert."

   - Siblings:
     "Stannis and Robert are siblings."
     "Renly is a brother of Stannis."
     "Cersei is a sister of Jaime."
     
   - Spouse (1 per person, male-female only):
     "Catelyn is the spouse of Ned."

   - Other Relatives (explicit):
     "Tywin is a grandfather of Joffrey."
     "Olenna is a grandmother of Margaery."
     "Kevan is an uncle of Tyrion."
     "Genna is an aunt of Jaime."

2. QUESTIONS
   End all questions with a question mark '?'.

   - Yes/No Questions:
     "Is Robert the father of Joffrey?"
     "Are Tyrion and Cersei siblings?"
     "Is Ned the spouse of Catelyn?"
     "Is Tywin a grandfather of Tommen?"
     "Are Joffrey and Myrcella cousins?"
     "Are Tywin and Joffrey relatives?"

   - "Who" Questions:
     "Who is the father of Jon?"
     "Who is the mother of Daenerys?"
     "Who are the parents of Arya?"
     "Who are the children of Ned?"
     "Who are the siblings of Sansa?"
     "Who are the brothers of Robb?"
     "Who are the sisters of Bran?"
     "Who are the sons of Catelyn?"
     "Who are the daughters of Ned?"
     "Who is the spouse of Ned?"

3. SYSTEM COMMANDS
   "help"    - Shows this help message.
   "status"  - Shows a summary of everyone I know about.
   "reset"   - Clears all facts from my memory.
   "clear"   - Clears the screen.
   "exit"    - Quits the application.
"""

def run_family_chatbot():
    """Main function to run the interactive Family Relationship Chatbot."""
    print("\n" + "=" * 70)
    print("  Welcome to FamiLink - Family Relationship Chatbot")
    print("  Type 'help' for detailed instructions or 'exit' to quit.")
    print("=" * 70)
    
    try:
        chatbot = FamilyRelationshipBot()
        while True:
            try:
                user_input = input("\n> ")
            except (KeyboardInterrupt, EOFError):
                print("\nExiting FamiLink. Goodbye!")
                break
            
            try:
                response = chatbot.execute_user_input(user_input)
                if response == "exit_command":
                    print("\nThank you for using FamiLink! Goodbye!")
                    break
                if response == "clear_screen":
                    os.system('cls' if os.name == 'nt' else 'clear')
                    continue
                print(response)
            except Exception as e:
                print(f"An unexpected error occurred.\nDetails: {str(e)}")

    except FileNotFoundError as e:
        print(f"FATAL ERROR: Required Prolog knowledge base file not found!\nDetails: {e}")
    except Exception as e:
        print(f"A fatal, unexpected error occurred during initialization: {e}")

if __name__ == "__main__":
    run_family_chatbot()
