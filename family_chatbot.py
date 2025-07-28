#!/usr/bin/env python3
"""
FamiLink Family Relationship Chatbot
"""

import os
import re
import sys
import time
import signal
from pyswip import Prolog
from typing import List, Optional, Dict, Any

class TimeoutError(Exception):
    pass

class time_limit:
    """Context manager for timing out function calls"""
    def __init__(self, seconds):
        self.seconds = seconds
        
    def __enter__(self):
        signal.signal(signal.SIGALRM, self.handle_timeout)
        signal.alarm(self.seconds)
        
    def __exit__(self, exc_type, exc_val, exc_tb):
        signal.alarm(0)
        
    def handle_timeout(self, signum, frame):
        raise TimeoutError()

class FamilyRelationshipBot:
    """
    Main chatbot class for processing family relationship statements and queries.
    Uses Prolog as the inference engine for logical reasoning about family connections.
    """

    def __init__(self, knowledge_file: str = "family_relationships.pl"):
        """
        Initialize the family relationship chatbot with Prolog knowledge base.
        
        Args:
            knowledge_file: Path to the Prolog file containing family relationship rules
        """
        self.prolog_engine = Prolog()
        
        # Verify and load the Prolog knowledge base
        if not os.path.exists(knowledge_file):
            raise FileNotFoundError(f"Knowledge base file not found: {knowledge_file}")
        
        self.prolog_engine.consult(knowledge_file)
        print(f"Family relationship knowledge base loaded from {knowledge_file}")
    
    def clear_all_facts(self):
        """Remove all dynamic facts from the knowledge base"""
        try:
            fact_types = [
                ("person_male", 1), ("person_female", 1), ("has_parent", 2), 
                ("explicit_sibling", 2), ("explicit_uncle", 2), ("explicit_aunt", 2), 
                ("explicit_grandparent", 2)
            ]
            
            for fact_type, parameter_count in fact_types:
                try:
                    if parameter_count == 1:
                        existing_facts = list(self.prolog_engine.query(f"{fact_type}(Person)"))
                        for fact in existing_facts:
                            self.prolog_engine.retract(f"{fact_type}({fact['Person']})")
                    else:  # parameter_count == 2
                        existing_facts = list(self.prolog_engine.query(f"{fact_type}(Person1, Person2)"))
                        for fact in existing_facts:
                            self.prolog_engine.retract(f"{fact_type}({fact['Person1']}, {fact['Person2']})")
                except Exception as e:
                    continue  # Skip if fact type doesn't exist or is empty
        except Exception:
            pass  # Reset operation might fail, continue anyway
    
    def is_valid_name_format(self, name: str) -> bool:
        """
        Check if name follows project specification format.
        
        Args:
            name: Name to validate
            
        Returns:
            True if name is valid (only letters, first capitalized, rest lowercase)
        """
        return (name.isalpha() and 
                len(name) > 0 and
                name[0].isupper() and 
                name[1:].islower())
    
    def standardize_name(self, name: str) -> str:
        """
        Convert name to standard format for processing.
        
        Args:
            name: Input name
            
        Returns:
            Name in proper case format
        """
        return name.strip().capitalize()
    
    def execute_user_input(self, user_input: str) -> str:
        """
        Main method to process user input and return appropriate response.
        
        Args:
            user_input: Natural language input from user
            
        Returns:
            Response string based on input type and content
        """
        # Clean and normalize input
        user_input = user_input.strip()
        
        # Handle empty input
        if not user_input:
            return "Please enter a family relationship statement or question."
        
        # Process special commands
        if user_input.lower() in ["exit", "quit", "goodbye"]:
            return "exit_command"
        
        if user_input.lower() == "help":
            return self.display_help_information()
        
        if user_input.lower() == "status":
            return self.show_knowledge_base_summary()
        
        if user_input.lower() == "reset":
            self.clear_all_facts()
            return "All family relationship data has been cleared."
        
        if user_input.lower() in ["clear", "cls"]:
            return "clear_screen"
        
        # Route to appropriate processor based on input type
        if user_input.endswith("?"):
            return self.process_family_question(user_input)
        else:
            return self.process_family_statement(user_input)
    
    def process_family_statement(self, statement: str) -> str:
        """
        Process statements about family relationships.
        
        Args:
            statement: Family relationship statement
            
        Returns:
            Confirmation or error message
        """
        # Remove trailing period if present
        statement = statement.rstrip(".")
        
        # Define statement pattern handlers
        statement_handlers = [
            # Parent-child relationships
            (r"(\w+) is the father of (\w+)", self.handle_father_statement),
            (r"(\w+) is the mother of (\w+)", self.handle_mother_statement),
            (r"(\w+) and (\w+) are the parents of (\w+)", self.handle_parents_statement),
            
            # Child relationships
            (r"(\w+) is a child of (\w+)", self.handle_child_statement),
            (r"(\w+) is a son of (\w+)", self.handle_son_statement),
            (r"(\w+) is a daughter of (\w+)", self.handle_daughter_statement),
            
            # Multiple children patterns
            (r"(\w+) and (\w+) are children of (\w+)", self.handle_two_children_statement),
            (r"(\w+), (\w+), and (\w+) are children of (\w+)", self.handle_three_children_statement),
            (r"(\w+), (\w+), (\w+), and (\w+) are children of (\w+)", self.handle_four_children_statement),
            
            # Sibling relationships
            (r"(\w+) and (\w+) are siblings", self.handle_siblings_statement),
            (r"(\w+) is a brother of (\w+)", self.handle_brother_statement),
            (r"(\w+) is a sister of (\w+)", self.handle_sister_statement),
            
            # Grandparent relationships
            (r"(\w+) is a grandfather of (\w+)", self.handle_grandfather_statement),
            (r"(\w+) is a grandmother of (\w+)", self.handle_grandmother_statement),
            
            # Extended family relationships
            (r"(\w+) is an uncle of (\w+)", self.handle_uncle_statement),
            (r"(\w+) is an aunt of (\w+)", self.handle_aunt_statement),
        ]
        
        # Try to match statement against patterns
        for pattern, handler_method in statement_handlers:
            pattern_match = re.match(pattern, statement, re.IGNORECASE)
            if pattern_match:
                captured_groups = pattern_match.groups()
                
                # Validate all names in the statement
                for name in captured_groups:
                    if not self.is_valid_name_format(name):
                        return "Invalid name format. Names must be letters only, starting with uppercase."
                
                # Standardize names and call appropriate handler
                standardized_names = [self.standardize_name(name) for name in captured_groups]
                return handler_method(*standardized_names)
        
        # No pattern matched
        return "I don't understand that statement format. Please check the help for valid patterns."
    
    def process_family_question(self, question: str) -> str:
        """
        Process questions about family relationships.
        
        Args:
            question: Family relationship question
            
        Returns:
            Answer to the question
        """
        # Remove question mark
        question = question.rstrip("?")
        
        # Define question pattern handlers
        question_handlers = [
            # Yes/No questions
            (r"is (\w+) the father of (\w+)", self.answer_father_question),
            (r"is (\w+) the mother of (\w+)", self.answer_mother_question),
            (r"are (\w+) and (\w+) the parents of (\w+)", self.answer_parents_question),
            (r"is (\w+) a child of (\w+)", self.answer_child_question),
            (r"are (\w+) and (\w+) children of (\w+)", self.answer_children_question),
            (r"is (\w+) a son of (\w+)", self.answer_son_question),
            (r"is (\w+) a daughter of (\w+)", self.answer_daughter_question),
            (r"are (\w+) and (\w+) siblings", self.answer_siblings_question),
            (r"is (\w+) a brother of (\w+)", self.answer_brother_question),
            (r"is (\w+) a sister of (\w+)", self.answer_sister_question),
            (r"is (\w+) a grandfather of (\w+)", self.answer_grandfather_question),
            (r"is (\w+) a grandmother of (\w+)", self.answer_grandmother_question),
            (r"is (\w+) an uncle of (\w+)", self.answer_uncle_question),
            (r"is (\w+) an aunt of (\w+)", self.answer_aunt_question),
            (r"are (\w+) and (\w+) relatives", self.answer_relatives_question),
            
            # Information questions
            (r"who is the father of (\w+)", self.find_father),
            (r"who is the mother of (\w+)", self.find_mother),
            (r"who are the parents of (\w+)", self.find_parents),
            (r"who are the children of (\w+)", self.find_children),
            (r"who are the siblings of (\w+)", self.find_siblings),
            (r"who are the brothers of (\w+)", self.find_brothers),
            (r"who are the sisters of (\w+)", self.find_sisters),
            (r"who are the daughters of (\w+)", self.find_daughters),
            (r"who are the sons of (\w+)", self.find_sons),
        ]
        
        # Try to match question against patterns
        for pattern, handler_method in question_handlers:
            pattern_match = re.match(pattern, question, re.IGNORECASE)
            if pattern_match:
                captured_groups = pattern_match.groups()
                
                # Validate all names in the question
                for name in captured_groups:
                    if not self.is_valid_name_format(name):
                        return "Invalid name format. Names must be letters only, starting with uppercase."
                
                # Standardize names and call appropriate handler
                standardized_names = [self.standardize_name(name) for name in captured_groups]
                return handler_method(*standardized_names)
        
        # No pattern matched
        return "I don't understand that question format. Please check the help for valid patterns."
    
    def verify_relationship_exists(self, query: str) -> bool:
        """Check if a relationship exists with timeout protection"""
        try:
            # Add this context manager (requires time_limit implementation)
            with time_limit(5):  # 5-second timeout
                query_results = []
                
                for result in self.prolog_engine.query(query):
                    query_results.append(result)
                    if len(query_results) > 0:  # Early exit if we find a match
                        break
                
                return len(query_results) > 0
                
        except TimeoutError:
            return False
        except Exception:
            return False
    
    def detect_logical_errors(self) -> bool:
        """
        Check for logical inconsistencies in the knowledge base.
        
        Returns:
            True if errors detected, False otherwise
        """
        try:
            # Add specific error checks with timeouts
            error_types = [
                "self_relation", "circular_family", "three_gen_loop", "four_gen_loop",
                "ancestor_loop", "gender_mismatch", "invalid_father_gender", 
                "invalid_mother_gender", "invalid_son_gender", "invalid_daughter_gender",
                "invalid_brother_gender", "invalid_sister_gender", 
                "invalid_grandfather_gender", "invalid_grandmother_gender",
                "invalid_uncle_gender", "invalid_aunt_gender", 
                "too_many_biological_parents", "multiple_biological_fathers",
                "multiple_biological_mothers", "sibling_parent_impossible",
                "parent_grandparent_impossible", "uncle_parent_impossible",
                "aunt_parent_impossible", "sibling_grandparent_impossible",
                "person_their_own_ancestor", "person_their_own_descendant",
                "cousin_grandparent_impossible", "relatives_having_child",
                "siblings_having_child", "parent_child_having_child",
                "siblings_without_shared_parent", "sibling_ancestor_impossible",
                "self_cousin", "parent_child_as_cousins", "generation_gap_too_large"
            ]
            
            for error_type in error_types:
                try:
                    # Set a timeout for each specific error check
                    start_time = time.time()
                    for _ in self.prolog_engine.query(f"logical_error({error_type})"):
                        if time.time() - start_time > 1.0:  # 1 second timeout
                            return True  # Consider timeout as detecting an error
                        return True  # Error found
                except:
                    continue  # Skip to the next error type if one fails
            
            return False  # No errors detected
        except Exception:
            return True  # Assume error on exception for safety

    def safely_add_facts(self, fact_list: List[str]) -> bool:
        """
        Add multiple facts to knowledge base with rollback on error.
        
        Args:
            fact_list: List of Prolog facts to add
            
        Returns:
            True if all facts added successfully, False if errors detected
        """
        added_facts = []
        
        try:
            # First check if adding these facts would create circular relationships
            for fact in fact_list:
                if "has_parent" in fact:
                    params = fact[fact.find("(")+1:fact.find(")")]
                    child, parent = params.split(", ")
                    
                    # Clean the quotes from the parameters
                    child = child.replace("'", "")
                    parent = parent.replace("'", "")
                    
                    # Check if adding this relationship would create a circular family tree
                    # by seeing if the "child" is already an ancestor of the "parent"
                    if parent != child:  # Skip self-relation check, handled by Prolog
                        check_query = f"has_ancestor('{parent}', '{child}')"
                        if self.verify_relationship_exists(check_query):
                            return False  # Would create a circular relationship
            
            # Separate sibling facts from other facts
            sibling_facts = [f for f in fact_list if "explicit_sibling" in f]
            other_facts = [f for f in fact_list if "explicit_sibling" not in f]
            
            # First process non-sibling facts
            for fact in other_facts:
                # Skip if fact already exists
                if not self.fact_already_exists(fact):
                    self.prolog_engine.assertz(fact)
                    added_facts.append(fact)
                
                # Check for logical errors after each addition
                if self.detect_logical_errors():
                    # Rollback all added facts
                    for rollback_fact in added_facts:
                        try:
                            self.prolog_engine.retract(rollback_fact)
                        except:
                            pass
                    return False
            
            # Temporarily store sibling facts to check for errors
            temp_added_siblings = []
            for fact in sibling_facts:
                # Skip if fact already exists
                if not self.fact_already_exists(fact):
                    self.prolog_engine.assertz(fact)
                    temp_added_siblings.append(fact)
                    added_facts.append(fact)  # Add to main list for potential rollback
            
            # Check for logical errors after adding all sibling facts
            if temp_added_siblings and self.detect_logical_errors():
                # Rollback all facts (both non-sibling and sibling) because we are in one transaction
                for rollback_fact in added_facts:
                    try:
                        self.prolog_engine.retract(rollback_fact)
                    except:
                        pass
                return False
            
            return True
        except Exception as e:
            # Rollback on any exception
            for rollback_fact in added_facts:
                try:
                    self.prolog_engine.retract(rollback_fact)
                except:
                    pass
            return False
    
    def fact_already_exists(self, fact: str) -> bool:
        """Check if a specific fact already exists in the knowledge base"""
        try:
            # Extract predicate and parameters
            if "has_parent(" in fact:
                params = fact[fact.find("(")+1:fact.find(")")]
                return self.verify_relationship_exists(f"has_parent({params})")
            elif "person_male(" in fact:
                params = fact[fact.find("(")+1:fact.find(")")]
                return self.verify_relationship_exists(f"person_male({params})")
            elif "person_female(" in fact:
                params = fact[fact.find("(")+1:fact.find(")")]
                return self.verify_relationship_exists(f"person_female({params})")
            elif "explicit_sibling(" in fact:
                params = fact[fact.find("(")+1:fact.find(")")]
                return self.verify_relationship_exists(f"explicit_sibling({params})")
            return False
        except:
            return False
    
    # Statement handler methods
    
    def handle_father_statement(self, father_name: str, child_name: str) -> str:
        """Handle father-child relationship statement"""
        if self.verify_relationship_exists(f"is_dad('{father_name}', '{child_name}')"):
            return "I already knew that."
        
        facts_to_add = [
            f"has_parent('{child_name}', '{father_name}')",
            f"person_male('{father_name}')"
        ]
        
        if self.verify_relationship_exists(f"person_female('{father_name}')"):
            return "That's impossible!"
        if self.safely_add_facts(facts_to_add):
            return "OK! I learned something."
        else:
            return "That's impossible!"
        
    def handle_mother_statement(self, mother_name: str, child_name: str) -> str:
        """Handle mother-child relationship statement"""
        if self.verify_relationship_exists(f"is_mom('{mother_name}', '{child_name}')"):
            return "I already knew that."
        
        facts_to_add = [
            f"has_parent('{child_name}', '{mother_name}')",
            f"person_female('{mother_name}')"
        ]
        
        if self.verify_relationship_exists(f"person_male('{mother_name}')"):
            return "That's impossible!"
        if self.safely_add_facts(facts_to_add):
            return "OK! I learned something."
        else:
            return "That's impossible!"
    
    def handle_parents_statement(self, parent1_name: str, parent2_name: str, child_name: str) -> str:
        """Handle parents-child relationship statement"""
        existing1 = self.verify_relationship_exists(f"has_parent('{child_name}', '{parent1_name}')")
        existing2 = self.verify_relationship_exists(f"has_parent('{child_name}', '{parent2_name}')")
        
        if existing1 and existing2:
            return "I already knew that."
        
        facts_to_add = []
        if not existing1:
            facts_to_add.append(f"has_parent('{child_name}', '{parent1_name}')")
        if not existing2:
            facts_to_add.append(f"has_parent('{child_name}', '{parent2_name}')")
            
        if self.safely_add_facts(facts_to_add):
            return "OK! I learned something."
        else:
            return "That's impossible!"
    
    def handle_child_statement(self, child_name: str, parent_name: str) -> str:
        """Handle child-parent relationship statement"""
        if self.verify_relationship_exists(f"has_parent('{child_name}', '{parent_name}')"):
            return "I already knew that."
        
        if self.safely_add_facts([f"has_parent('{child_name}', '{parent_name}')"]):
            return "OK! I learned something."
        else:
            return "That's impossible!"
    
    def handle_son_statement(self, son_name: str, parent_name: str) -> str:
        """Handle son-parent relationship statement"""
        if self.verify_relationship_exists(f"is_male_child('{son_name}', '{parent_name}')"):
            return "I already knew that."
        
        facts_to_add = [
            f"has_parent('{son_name}', '{parent_name}')",
            f"person_male('{son_name}')"
        ]
        
        if self.safely_add_facts(facts_to_add):
            return "OK! I learned something."
        else:
            return "That's impossible!"
    
    def handle_daughter_statement(self, daughter_name: str, parent_name: str) -> str:
        """Handle daughter-parent relationship statement"""
        if self.verify_relationship_exists(f"is_female_child('{daughter_name}', '{parent_name}')"):
            return "I already knew that."
        
        facts_to_add = [
            f"has_parent('{daughter_name}', '{parent_name}')",
            f"person_female('{daughter_name}')"
        ]
        
        if self.safely_add_facts(facts_to_add):
            return "OK! I learned something."
        else:
            return "That's impossible!"
    
    def handle_two_children_statement(self, child1_name: str, child2_name: str, parent_name: str) -> str:
        """Handle two children-parent relationship statement"""
        existing1 = self.verify_relationship_exists(f"has_parent('{child1_name}', '{parent_name}')")
        existing2 = self.verify_relationship_exists(f"has_parent('{child2_name}', '{parent_name}')")
        
        if existing1 and existing2:
            return "I already knew that."
        
        facts_to_add = []
        if not existing1:
            facts_to_add.append(f"has_parent('{child1_name}', '{parent_name}')")
        if not existing2:
            facts_to_add.append(f"has_parent('{child2_name}', '{parent_name}')")
        
        if self.safely_add_facts(facts_to_add):
            return "OK! I learned something."
        else:
            return "That's impossible!"
    
    def handle_three_children_statement(self, child1_name: str, child2_name: str, child3_name: str, parent_name: str) -> str:
        """Handle three children-parent relationship statement"""
        existing1 = self.verify_relationship_exists(f"has_parent('{child1_name}', '{parent_name}')")
        existing2 = self.verify_relationship_exists(f"has_parent('{child2_name}', '{parent_name}')")
        existing3 = self.verify_relationship_exists(f"has_parent('{child3_name}', '{parent_name}')")
        
        if existing1 and existing2 and existing3:
            return "I already knew that."
        
        facts_to_add = []
        if not existing1:
            facts_to_add.append(f"has_parent('{child1_name}', '{parent_name}')")
        if not existing2:
            facts_to_add.append(f"has_parent('{child2_name}', '{parent_name}')")
        if not existing3:
            facts_to_add.append(f"has_parent('{child3_name}', '{parent_name}')")
        
        if self.safely_add_facts(facts_to_add):
            return "OK! I learned something."
        else:
            return "That's impossible!"
    
    def handle_four_children_statement(self, child1_name: str, child2_name: str, child3_name: str, child4_name: str, parent_name: str) -> str:
        """Handle four children-parent relationship statement"""
        existing1 = self.verify_relationship_exists(f"has_parent('{child1_name}', '{parent_name}')")
        existing2 = self.verify_relationship_exists(f"has_parent('{child2_name}', '{parent_name}')")
        existing3 = self.verify_relationship_exists(f"has_parent('{child3_name}', '{parent_name}')")
        existing4 = self.verify_relationship_exists(f"has_parent('{child4_name}', '{parent_name}')")
        
        if existing1 and existing2 and existing3 and existing4:
            return "I already knew that."
        
        facts_to_add = []
        if not existing1:
            facts_to_add.append(f"has_parent('{child1_name}', '{parent_name}')")
        if not existing2:
            facts_to_add.append(f"has_parent('{child2_name}', '{parent_name}')")
        if not existing3:
            facts_to_add.append(f"has_parent('{child3_name}', '{parent_name}')")
        if not existing4:
            facts_to_add.append(f"has_parent('{child4_name}', '{parent_name}')")
        
        if self.safely_add_facts(facts_to_add):
            return "OK! I learned something."
        else:
            return "That's impossible!"
    
    def handle_siblings_statement(self, sibling1_name: str, sibling2_name: str) -> str:
        """Handle siblings relationship statement without immediate parent check"""
        if self.verify_relationship_exists(f"are_siblings('{sibling1_name}', '{sibling2_name}')"):
            return "I already knew that."
        
        facts_to_add = [
            f"explicit_sibling('{sibling1_name}', '{sibling2_name}')",
            f"explicit_sibling('{sibling2_name}', '{sibling1_name}')"
        ]
        
        if self.safely_add_facts(facts_to_add):
            return "OK! I learned something."
        else:
            return "That's impossible!"

    def handle_brother_statement(self, brother_name: str, sibling_name: str) -> str:
        """Handle brother relationship statement without immediate parent check"""
        if self.verify_relationship_exists(f"is_male_sibling('{brother_name}', '{sibling_name}')"):
            return "I already knew that."
        
        facts_to_add = [
            f"explicit_sibling('{brother_name}', '{sibling_name}')",
            f"explicit_sibling('{sibling_name}', '{brother_name}')",
            f"person_male('{brother_name}')"
        ]
        
        if self.safely_add_facts(facts_to_add):
            return "OK! I learned something."
        else:
            return "That's impossible!"

    def handle_sister_statement(self, sister_name: str, sibling_name: str) -> str:
        """Handle sister relationship statement without immediate parent check"""
        if self.verify_relationship_exists(f"is_female_sibling('{sister_name}', '{sibling_name}')"):
            return "I already knew that."
        
        facts_to_add = [
            f"explicit_sibling('{sister_name}', '{sibling_name}')",
            f"explicit_sibling('{sibling_name}', '{sister_name}')",
            f"person_female('{sister_name}')"
        ]
        
        if self.safely_add_facts(facts_to_add):
            return "OK! I learned something."
        else:
            return "That's impossible!"
    
    def handle_grandfather_statement(self, grandfather_name: str, grandchild_name: str) -> str:
        """Handle grandfather relationship statement"""
        if self.verify_relationship_exists(f"is_grandfather('{grandfather_name}', '{grandchild_name}')"):
            return "I already knew that."
        
        facts_to_add = [
            f"explicit_grandparent('{grandfather_name}', '{grandchild_name}')",
            f"person_male('{grandfather_name}')"
        ]
        
        if self.safely_add_facts(facts_to_add):
            return "OK! I learned something."
        else:
            return "That's impossible!"
    
    def handle_grandmother_statement(self, grandmother_name: str, grandchild_name: str) -> str:
        """Handle grandmother relationship statement"""
        if self.verify_relationship_exists(f"is_grandmother('{grandmother_name}', '{grandchild_name}')"):
            return "I already knew that."
        
        facts_to_add = [
            f"explicit_grandparent('{grandmother_name}', '{grandchild_name}')",
            f"person_female('{grandmother_name}')"
        ]
        
        if self.safely_add_facts(facts_to_add):
            return "OK! I learned something."
        else:
            return "That's impossible!"
    
    def handle_uncle_statement(self, uncle_name: str, nephew_niece_name: str) -> str:
        """Handle uncle relationship statement"""
        if self.verify_relationship_exists(f"is_uncle_of('{uncle_name}', '{nephew_niece_name}')"):
            return "I already knew that."
        
        facts_to_add = [
            f"explicit_uncle('{uncle_name}', '{nephew_niece_name}')",
            f"person_male('{uncle_name}')"
        ]
        
        if self.safely_add_facts(facts_to_add):
            return "OK! I learned something."
        else:
            return "That's impossible!"
    
    def handle_aunt_statement(self, aunt_name: str, nephew_niece_name: str) -> str:
        """Handle aunt relationship statement"""
        if self.verify_relationship_exists(f"is_aunt_of('{aunt_name}', '{nephew_niece_name}')"):
            return "I already knew that."
        
        facts_to_add = [
            f"explicit_aunt('{aunt_name}', '{nephew_niece_name}')",
            f"person_female('{aunt_name}')"
        ]
        
        if self.safely_add_facts(facts_to_add):
            return "OK! I learned something."
        else:
            return "That's impossible!"
    
    # Question answer methods
    
    def answer_father_question(self, father_name: str, child_name: str) -> str:
        """Answer father relationship question"""
        try:
            result = self.verify_relationship_exists(f"is_dad('{father_name}', '{child_name}')")
            return "Yes" if result else "No"
        except Exception:
            return "No"
    
    def answer_mother_question(self, mother_name: str, child_name: str) -> str:
        """Answer mother relationship question"""
        try:
            result = self.verify_relationship_exists(f"is_mom('{mother_name}', '{child_name}')")
            return "Yes" if result else "No"
        except Exception:
            return "No"
    
    def answer_parents_question(self, parent1_name: str, parent2_name: str, child_name: str) -> str:
        """Answer parents relationship question"""
        try:
            result1 = self.verify_relationship_exists(f"has_parent('{child_name}', '{parent1_name}')")
            result2 = self.verify_relationship_exists(f"has_parent('{child_name}', '{parent2_name}')")
            return "Yes" if result1 and result2 else "No"
        except Exception:
            return "No"
    
    def answer_child_question(self, child_name: str, parent_name: str) -> str:
        """Answer child relationship question"""
        try:
            result = self.verify_relationship_exists(f"has_child('{parent_name}', '{child_name}')")
            return "Yes" if result else "No"
        except Exception:
            return "No"
    
    def answer_children_question(self, child1_name: str, child2_name: str, parent_name: str) -> str:
        """Answer children relationship question"""
        try:
            result1 = self.verify_relationship_exists(f"has_child('{parent_name}', '{child1_name}')")
            result2 = self.verify_relationship_exists(f"has_child('{parent_name}', '{child2_name}')")
            return "Yes" if result1 and result2 else "No"
        except Exception:
            return "No"
    
    def answer_son_question(self, son_name: str, parent_name: str) -> str:
        """Answer son relationship question"""
        try:
            result = self.verify_relationship_exists(f"is_male_child('{son_name}', '{parent_name}')")
            return "Yes" if result else "No"
        except Exception:
            return "No"
    
    def answer_daughter_question(self, daughter_name: str, parent_name: str) -> str:
        """Answer daughter relationship question"""
        try:
            result = self.verify_relationship_exists(f"is_female_child('{daughter_name}', '{parent_name}')")
            return "Yes" if result else "No"
        except Exception:
            return "No"
    
    def answer_siblings_question(self, sibling1_name: str, sibling2_name: str) -> str:
        """Answer siblings relationship question"""
        try:
            result = self.verify_relationship_exists(f"are_siblings('{sibling1_name}', '{sibling2_name}')")
            return "Yes" if result else "No"
        except Exception:
            return "No"
    
    def answer_brother_question(self, brother_name: str, sibling_name: str) -> str:
        """Answer brother relationship question"""
        try:
            result = self.verify_relationship_exists(f"is_male_sibling('{brother_name}', '{sibling_name}')")
            return "Yes" if result else "No"
        except Exception:
            return "No"
    
    def answer_sister_question(self, sister_name: str, sibling_name: str) -> str:
        """Answer sister relationship question"""
        try:
            result = self.verify_relationship_exists(f"is_female_sibling('{sister_name}', '{sibling_name}')")
            return "Yes" if result else "No"
        except Exception:
            return "No"
    
    def answer_grandfather_question(self, grandfather_name: str, grandchild_name: str) -> str:
        """Answer grandfather relationship question"""
        try:
            result = self.verify_relationship_exists(f"is_grandfather('{grandfather_name}', '{grandchild_name}')")
            return "Yes" if result else "No"
        except Exception:
            return "No"
    
    def answer_grandmother_question(self, grandmother_name: str, grandchild_name: str) -> str:
        """Answer grandmother relationship question"""
        try:
            result = self.verify_relationship_exists(f"is_grandmother('{grandmother_name}', '{grandchild_name}')")
            return "Yes" if result else "No"
        except Exception:
            return "No"
    
    def answer_uncle_question(self, uncle_name: str, nephew_niece_name: str) -> str:
        """Answer uncle relationship question"""
        try:
            result = self.verify_relationship_exists(f"is_uncle_of('{uncle_name}', '{nephew_niece_name}')")
            return "Yes" if result else "No"
        except Exception:
            return "No"
    
    def answer_aunt_question(self, aunt_name: str, nephew_niece_name: str) -> str:
        """Answer aunt relationship question"""
        try:
            result = self.verify_relationship_exists(f"is_aunt_of('{aunt_name}', '{nephew_niece_name}')")
            return "Yes" if result else "No"
        except Exception:
            return "No"
    
    def answer_relatives_question(self, person1_name: str, person2_name: str) -> str:
        """Answer relatives relationship question with comprehensive relationship checking"""
        if person1_name == person2_name:
            return "Yes"
        
        try:
            # Check simple direct relationships first (for efficiency)
            direct_queries = [
                f"has_parent('{person1_name}', '{person2_name}')",
                f"has_parent('{person2_name}', '{person1_name}')",
                f"are_siblings('{person1_name}', '{person2_name}')",
                f"has_grandparent('{person1_name}', '{person2_name}')",
                f"has_grandparent('{person2_name}', '{person1_name}')",
                f"is_uncle_of('{person1_name}', '{person2_name}')",
                f"is_uncle_of('{person2_name}', '{person1_name}')",
                f"is_aunt_of('{person1_name}', '{person2_name}')",
                f"is_aunt_of('{person2_name}', '{person1_name}')"
            ]
            
            for query in direct_queries:
                if self.verify_relationship_exists(query):
                    return "Yes"
            
            # Check for common ancestor relationship (covers cousins, etc.)
            common_ancestor_query = (
                f"common_ancestor('{person1_name}', '{person2_name}', _)"
            )
            
            if self.verify_relationship_exists(common_ancestor_query):
                return "Yes"
            
            # Add special check for grandparent-grandchild relationships through an intermediate parent
            gp_relation_query = (
                f"has_parent('{person1_name}', P), has_parent(P, '{person2_name}')"
            )
            if self.verify_relationship_exists(gp_relation_query):
                return "Yes"
            
            gp_relation_query = (
                f"has_parent('{person2_name}', P), has_parent(P, '{person1_name}')"
            )
            if self.verify_relationship_exists(gp_relation_query):
                return "Yes"
            
            # Check great-grandparent relationship explicitly
            ggp_relation_query = (
                f"has_parent('{person1_name}', P1), has_parent(P1, P2), has_parent(P2, '{person2_name}')"
            )
            if self.verify_relationship_exists(ggp_relation_query):
                return "Yes"
            
            ggp_relation_query = (
                f"has_parent('{person2_name}', P1), has_parent(P1, P2), has_parent(P2, '{person1_name}')"
            )
            if self.verify_relationship_exists(ggp_relation_query):
                return "Yes"
            
            # Check for cousin relationship explicitly
            cousin_query = (
                f"has_parent('{person1_name}', P1), "
                f"has_parent('{person2_name}', P2), "
                f"are_siblings(P1, P2)"
            )
            if self.verify_relationship_exists(cousin_query):
                return "Yes"
            
            # Check for second-cousin relationship explicitly
            second_cousin_query = (
                f"has_parent('{person1_name}', P1), "
                f"has_parent('{person2_name}', P2), "
                f"has_parent(P1, GP1), "
                f"has_parent(P2, GP2), "
                f"are_siblings(GP1, GP2)"
            )
            if self.verify_relationship_exists(second_cousin_query):
                return "Yes"
            
            # Final fallback check
            if self.verify_relationship_exists(f"family_related('{person1_name}', '{person2_name}')"):
                return "Yes"
                
            return "No"
        except Exception:
            return "No"
    
    # Information finder methods
    
    def find_father(self, child_name: str) -> str:
        """Find and return the father of a child"""
        try:
            query = f"is_dad(FatherName, '{child_name}')"
            query_results = list(self.prolog_engine.query(query))
            
            if not query_results:
                return "No one"
            
            fathers = list(set([result["FatherName"] for result in query_results]))
            return fathers[0] if len(fathers) == 1 else "No one"
        except Exception:
            return "No one"
    
    def find_mother(self, child_name: str) -> str:
        """Find and return the mother of a child"""
        try:
            query = f"is_mom(MotherName, '{child_name}')"
            query_results = list(self.prolog_engine.query(query))
            
            if not query_results:
                return "No one"
            
            mothers = list(set([result["MotherName"] for result in query_results]))
            return mothers[0] if len(mothers) == 1 else "No one"
        except Exception:
            return "No one"
    
    def find_parents(self, child_name: str) -> str:
        """Find and return the parents of a child"""
        try:
            query = f"has_parent('{child_name}', ParentName)"
            query_results = list(self.prolog_engine.query(query))
            
            if not query_results:
                return "No one"
            
            parents = sorted(list(set([result["ParentName"] for result in query_results])))
            if len(parents) == 1:
                return parents[0]
            elif len(parents) == 2:
                return f"{parents[0]} and {parents[1]}"
            else:
                return "No one"  # Should not happen in valid family relationships
        except Exception:
            return "No one"
    
    def find_children(self, parent_name: str) -> str:
        """Find and return the children of a parent"""
        try:
            query = f"has_child('{parent_name}', ChildName)"
            query_results = list(self.prolog_engine.query(query))
            
            if not query_results:
                return "No one"
            
            children = sorted(list(set([result["ChildName"] for result in query_results])))
            if len(children) == 1:
                return children[0]
            elif len(children) == 2:
                return f"{children[0]} and {children[1]}"
            elif len(children) > 2:
                return f"{', '.join(children[:-1])}, and {children[-1]}"
            else:
                return "No one"
        except Exception:
            return "No one"
    
    def find_siblings(self, person_name: str) -> str:
        """Find and return the siblings of a person"""
        try:
            query = f"are_siblings(SiblingName, '{person_name}'), SiblingName \\= '{person_name}'"
            query_results = list(self.prolog_engine.query(query))
            
            if not query_results:
                return "No one"
            
            siblings = sorted(list(set([result["SiblingName"] for result in query_results])))
            if len(siblings) == 1:
                return siblings[0]
            elif len(siblings) == 2:
                return f"{siblings[0]} and {siblings[1]}"
            elif len(siblings) > 2:
                return f"{', '.join(siblings[:-1])}, and {siblings[-1]}"
            else:
                return "No one"
        except Exception:
            return "No one"
    
    def find_brothers(self, person_name: str) -> str:
        """Find and return the brothers of a person"""
        try:
            query = f"is_male_sibling(BrotherName, '{person_name}')"
            query_results = list(self.prolog_engine.query(query))
            
            if not query_results:
                return "No one"
            
            brothers = sorted(list(set([result["BrotherName"] for result in query_results])))
            if len(brothers) == 1:
                return brothers[0]
            elif len(brothers) == 2:
                return f"{brothers[0]} and {brothers[1]}"
            elif len(brothers) > 2:
                return f"{', '.join(brothers[:-1])}, and {brothers[-1]}"
            else:
                return "No one"
        except Exception:
            return "No one"
    
    def find_sisters(self, person_name: str) -> str:
        """Find and return the sisters of a person"""
        try:
            query = f"is_female_sibling(SisterName, '{person_name}')"
            query_results = list(self.prolog_engine.query(query))
            
            if not query_results:
                return "No one"
            
            sisters = sorted(list(set([result["SisterName"] for result in query_results])))
            if len(sisters) == 1:
                return sisters[0]
            elif len(sisters) == 2:
                return f"{sisters[0]} and {sisters[1]}"
            elif len(sisters) > 2:
                return f"{', '.join(sisters[:-1])}, and {sisters[-1]}"
            else:
                return "No one"
        except Exception:
            return "No one"
    
    def find_daughters(self, parent_name: str) -> str:
        """Find and return the daughters of a parent"""
        try:
            query = f"is_female_child(DaughterName, '{parent_name}')"
            query_results = list(self.prolog_engine.query(query))
            
            if not query_results:
                return "No one"
            
            daughters = sorted(list(set([result["DaughterName"] for result in query_results])))
            if len(daughters) == 1:
                return daughters[0]
            elif len(daughters) == 2:
                return f"{daughters[0]} and {daughters[1]}"
            elif len(daughters) > 2:
                return f"{', '.join(daughters[:-1])}, and {daughters[-1]}"
            else:
                return "No one"
        except Exception:
            return "No one"
    
    def find_sons(self, parent_name: str) -> str:
        """Find and return the sons of a parent"""
        try:
            query = f"is_male_child(SonName, '{parent_name}')"
            query_results = list(self.prolog_engine.query(query))
            
            if not query_results:
                return "No one"
            
            sons = sorted(list(set([result["SonName"] for result in query_results])))
            if len(sons) == 1:
                return sons[0]
            elif len(sons) == 2:
                return f"{sons[0]} and {sons[1]}"
            elif len(sons) > 2:
                return f"{', '.join(sons[:-1])}, and {sons[-1]}"
            else:
                return "No one"
        except Exception:
            return "No one"
    
    def show_knowledge_base_summary(self) -> str:
        """Display summary information about the current knowledge base"""
        try:
            # Count different relationship types
            parent_relationships = len(list(self.prolog_engine.query("has_parent(_, _)")))
            male_individuals = len(list(self.prolog_engine.query("person_male(_)")))
            female_individuals = len(list(self.prolog_engine.query("person_female(_)")))
            
            # Count unique sibling pairs (avoid double-counting)
            sibling_pairs = set()
            for result in self.prolog_engine.query("explicit_sibling(A, B), A @< B"):
                pair = tuple(sorted([result["A"], result["B"]]))
                sibling_pairs.add(pair)
            sibling_relationships = len(sibling_pairs)
            
            # Count more relationship types
            grandparent_relationships = len(list(self.prolog_engine.query("explicit_grandparent(_, _)")))
            uncle_relationships = len(list(self.prolog_engine.query("explicit_uncle(_, _)")))
            aunt_relationships = len(list(self.prolog_engine.query("explicit_aunt(_, _)")))
            
            summary = f"""Current Knowledge Base Summary:
                • Parent-child relationships: {parent_relationships}
                • Male individuals: {male_individuals}
                • Female individuals: {female_individuals}
                • Sibling relationships: {sibling_relationships}
                • Grandparent relationships: {grandparent_relationships}
                • Uncle relationships: {uncle_relationships}
                • Aunt relationships: {aunt_relationships}
                • Total stored facts: {parent_relationships + male_individuals + female_individuals + (sibling_relationships * 2) + grandparent_relationships + uncle_relationships + aunt_relationships}"""
            return summary
        except Exception as e:
            return f"Error retrieving knowledge base summary: {e}"
    
    def display_help_information(self) -> str:
        """Display comprehensive help information for users"""
        help_content = """FamiLink Family Relationship Chatbot - Help Guide

        SUPPORTED STATEMENT FORMATS:
        Family Relationships:
        • [Name] is the father of [Name].
        • [Name] is the mother of [Name].
        • [Name] and [Name] are the parents of [Name].
        • [Name] is a child of [Name].
        • [Name] is a son of [Name].
        • [Name] is a daughter of [Name].

        Sibling Relationships:
        • [Name] and [Name] are siblings.
        • [Name] is a brother of [Name].
        • [Name] is a sister of [Name].

        Extended Family:
        • [Name] is a grandfather of [Name].
        • [Name] is a grandmother of [Name].
        • [Name] is an uncle of [Name].
        • [Name] is an aunt of [Name].

        Multiple Children:
        • [Name] and [Name] are children of [Name].
        • [Name], [Name], and [Name] are children of [Name].
        • [Name], [Name], [Name], and [Name] are children of [Name].

        SUPPORTED QUESTION FORMATS:
        Yes/No Questions:
        • Is [Name] the father of [Name]?
        • Is [Name] the mother of [Name]?
        • Are [Name] and [Name] the parents of [Name]?
        • Is [Name] a child of [Name]?
        • Are [Name] and [Name] children of [Name]?
        • Is [Name] a son of [Name]?
        • Is [Name] a daughter of [Name]?
        • Are [Name] and [Name] siblings?
        • Is [Name] a brother of [Name]?
        • Is [Name] a sister of [Name]?
        • Is [Name] a grandfather of [Name]?
        • Is [Name] a grandmother of [Name]?
        • Is [Name] an uncle of [Name]?
        • Is [Name] an aunt of [Name]?
        • Are [Name] and [Name] relatives?

        Information Questions:
        • Who is the father of [Name]?
        • Who is the mother of [Name]?
        • Who are the parents of [Name]?
        • Who are the children of [Name]?
        • Who are the siblings of [Name]?
        • Who are the brothers of [Name]?
        • Who are the sisters of [Name]?
        • Who are the daughters of [Name]?
        • Who are the sons of [Name]?

        SYSTEM COMMANDS:
        • help - Show this help information
        • status - Display knowledge base summary
        • reset - Clear all stored family data
        • clear/cls - Clear the screen
        • exit/quit/goodbye - Exit the program

        NAME REQUIREMENTS:
        Names must follow these rules:
        - Only alphabetic characters (no numbers or symbols)
        - First letter must be uppercase
        - Remaining letters must be lowercase
        - No spaces allowed

        EXAMPLES:
        Valid names: John, Mary, Elizabeth, Michael
        Invalid names: john, MARY, Pat-rick, John Smith, Mary123

        INTELLIGENT FEATURES:
        The system can automatically infer family relationships and detect impossible 
        situations based on biological constraints and logical consistency. It recognizes
        genealogical relationships or connections through blood relation.

        BIOLOGICAL CONSTRAINTS:
        - Relatives cannot have children together
        - A person can only have one biological father and one biological mother
        - Siblings must share at least one biological parent
        - Gender consistency is enforced (fathers must be male, mothers female, etc.)
                """
        return help_content.strip()


def run_family_chatbot():
    """Main function to run the Family Relationship Chatbot"""
    print("\n" + "=" * 70)
    print("  Welcome to FamiLink - Untangling Family Trees, One Relation at a Time!")
    print("  Type 'help' for detailed instructions or 'exit' to quit.")
    print("=" * 70)
    
    try:
        chatbot = FamilyRelationshipBot()
        
        while True:
            user_input = input("\n> ")
            
            try:
                response = chatbot.execute_user_input(user_input)
                
                if response == "exit_command":
                    print("\nThank you for using FamiLink! Goodbye!")
                    break
                    
                if response == "clear_screen":
                    # Clear screen command for better UX
                    os.system('cls' if os.name == 'nt' else 'clear')
                    continue
                    
                print(response)
            except Exception as e:
                print("Sorry, I encountered an error processing your request.")
                print(f"Error details: {str(e)}")
                print("Please try a different query or type 'reset' to clear the knowledge base.")
    
    except FileNotFoundError:
        print("Error: Required Prolog knowledge base file 'family_relationships.pl' not found!")
        print("Please ensure both files are in the same directory.")
    except Exception as e:
        print(f"An unexpected error occurred: {e}")
        print("Please verify your SWI-Prolog installation and pyswip package.")


if __name__ == "__main__":
    run_family_chatbot()
