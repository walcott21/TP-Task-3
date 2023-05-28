:- dynamic ontologia/1, conceito/2, individuo/1, relacao/1, triplo/4.

% Here we insert new concept specification to our ontology, then we assert the name and attributes.
insert_concept_specification(Concept, Attributes) :-
    assertz(conceito(name(Concept), atribs(Attributes))).

% Here we insert new individual specification to our ontology, then we assert the name
insert_individual(Individual) :-
    assertz(individuo(Individual)).

% We do the same here for Relation
insert_relation(Relation) :-
    assertz(relacao(Relation)).

% Here we insert triple but we have to assert the subject, relation, and object
insert_triple(Subject, Relation, Object) :-
    assertz(triplo(Subject, Relation, Object, [])).

insert_triple(Subject, Relation, Object, Attributes) :-
    assertz(triplo(Subject, Relation, Object, Attributes)).

% Here we count the number of attributes that we have for a concept
count_attributes(Concept, Count) :-
    conceito(name(Concept), atribs(Attributes)),
    length(Attributes, Count).

% Here we generate the dot code, first we have the basic structure and the name of the file
generate_dot_code :-
    open('DotGraph.txt', write, Stream),
    write(Stream, 'digraph map {\n'),
    write_concept_nodes(Stream),
    write_individual_nodes(Stream),
    write_triplet_nodes(Stream),
    write(Stream, '}'),
    close(Stream).

% After structuring the the dotfile, here we detail how concept nodes will be written
write_concept_nodes(Stream) :-
    conceito(name(Concept), atribs(Attributes)),
    format(Stream, '"~w" [shape=ellipse, style=filled, color=turquoise4];\n', [Concept]),
    write_attributes(Stream, Concept, Attributes),
    fail.
write_concept_nodes(_).

write_attributes(_, _, []).
write_attributes(Stream, Concept, [(Name, _Value) | Rest]) :-
    atom_string(Name, NameStr),
    format(Stream, '"~w=’~w’" [shape=rectangle, color=goldenrod];\n', [Concept, NameStr]),
    format(Stream, '"~w"->"~w=’~w’" [label="properties", style=dotted, color=red];\n', [Concept, Concept, NameStr]),
    write_attributes(Stream, Concept, Rest).



% We then do the same thing with individual nodes
write_individual_nodes(Stream) :-
    individuo(Individual),
    format(Stream, '"~w" [shape=rectangle, style=filled, color=goldenrod];\n', [Individual]),
    fail.
write_individual_nodes(_).

% Again, same with triples
write_triplet_nodes(Stream) :-
    triplo(Subject, Relation, Object, AttributeList),
    format(Stream, '"~w"->"~w" [label="~w"];\n', [Subject, Object, Relation]),
    write_attributes(Stream, Subject, Object, AttributeList),
    fail.
write_triplet_nodes(_).

% Then the attributes
write_attributes(_, _, []).
write_attributes(Stream, Concept, [(Name, _Value) | Rest]) :-
    atom_string(Name, NameStr),
    format(Stream, '"~w=’~w’" [shape=rectangle, color=goldenrod];\n', [Concept, NameStr]),
    format(Stream, '"~w"->"~w=’~w’" [label="properties", style=dotted, color=red];\n', [Concept, Concept, NameStr]),
    write_attributes(Stream, Concept, Rest).



% Entry point to generate the DOT code and save it to a file
generate_ontology_graph :-
    generate_dot_code.

% Simple function to list all concepts
list_concepts :-
    writeln('Concepts:'),
    conceito(name(Concept), _),
    writeln(Concept),
    fail.
list_concepts.

% Same thing for individuals, here we use findall but it can also be done in recursive like other listing functions
list_individuals :-
    writeln('Individuals:'),
    findall(Individual, individuo(Individual), Individuals),
    writeln(Individuals).

% Again with relations
list_relations :-
    writeln('Relations:'),
    relacao(Relation),
    writeln(Relation),
    fail.
list_relations.

% Same for triples
list_triples :-
    writeln('Triples:'),
    triplo(Subject, Relation, Object, _),
    format('Subject: ~w, Relation: ~w, Object: ~w~n', [Subject, Relation, Object]),
    fail.
list_triples.

% Here we validate the triples we have by making sure that they are formed correctly.
validate_existing_triples :-
    writeln('Valid Triples:'),
    validate_valid_triples,
    writeln('Invalid Triples:'),
    validate_invalid_triples.

% Validatation to find valid triples
validate_valid_triples :-
    triplo(Subject, Relation, Object, _),
    validate_triple(Subject, Relation, Object),
    format('Subject: ~w, Relation: ~w, Object: ~w~n', [Subject, Relation, Object]),
    fail.
validate_valid_triples.

% Validation to find invalid triples
validate_invalid_triples :-
    triplo(Subject, Relation, Object, _),
    \+ validate_triple(Subject, Relation, Object),
    format('Subject: ~w, Relation: ~w, Object: ~w~n', [Subject, Relation, Object]),
    fail.
validate_invalid_triples.

% This is the function used to basically check if the rules are applied and structure is correct
validate_triple(Subject, Relation, Object) :-
    (
        (individuo(Subject); (conceito(name(Subject), _), Subject \= name(_))),
        relacao(Relation),
        (individuo(Object); (conceito(name(Object), _), Object \= name(_)))
    ).

% Here we insert our Ontology data
initialize_ontology_data :-
    % Insert concept specifications
    insert_concept_specification(city, [(name, string)]),
    insert_concept_specification(traveler, [(name, string)]),

    % Insert individuals
    insert_individual(pl),
    insert_individual(ine),
    insert_individual(pt),
    insert_individual(lx),

    % Insert relations
    insert_relation(alreadyVisited),
    insert_relation(lives),
    insert_relation(iof),

    % Insert triples
    insert_triple(pl, iof, traveler),
    insert_triple(ine, iof, traveler),
    insert_triple(pt, iof, city),
    insert_triple(lx, iof, city),
    insert_triple(pl, alreadyVisited, pt),
    insert_triple(ine, alreadyVisited, lx),
    insert_triple(pl, lives, lx),
    insert_triple(ine, lives, pt).

% Predicate to initialize the program
init :-
    initialize_ontology_data,
    menu.

% Main menu
menu :-
    writeln('1. Insert Concept Specification'),
    writeln('2. Insert Individual'),
    writeln('3. Insert Relation'),
    writeln('4. Insert Triple'),
    writeln('5. Count Attributes'),
    writeln('6. Generate DOT Code'),
    writeln('7. List Concepts'),
    writeln('8. List Individuals'),
    writeln('9. List Relations'),
    writeln('10. List Triples'),
    writeln('11. Validate Existing Triples'),
    writeln('0. Exit'),
    read_option.

% Here we take the option from the user
read_option :-
    writeln('Please choose an option:'),
    read(Option),
    process_option(Option).

% Processing the chosen option
process_option(1) :-
    writeln('Enter the concept name:'),
    read(Concept),
    writeln('Enter the attributes (in the format [(Name, Type)]):'),
    read(Attributes),
    insert_concept_specification(Concept, Attributes),
    writeln('Concept specification inserted.'),
    menu.
process_option(2) :-
    writeln('Enter the individual name:'),
    read(Individual),
    insert_individual(Individual),
    writeln('Individual inserted.'),
    menu.
process_option(3) :-
    writeln('Enter the relation name:'),
    read(Relation),
    insert_relation(Relation),
    writeln('Relation inserted.'),
    menu.
process_option(4) :-
    writeln('Enter the subject:'),
    read(Subject),
    writeln('Enter the relation:'),
    read(Relation),
    writeln('Enter the object:'),
    read(Object),
    process_triple_input(Subject, Relation, Object).

process_triple_input(Subject, Relation, Object) :-
    writeln('Do you want to specify attributes for the triple? (y/n)'),
    read(Choice),
    (
        Choice = 'y' -> process_triple_with_attributes(Subject, Relation, Object);
        process_triple_without_attributes(Subject, Relation, Object)
    ).

process_triple_with_attributes(Subject, Relation, Object) :-
    writeln('Enter the attributes (in the format [(Name, Value)]):'),
    read(Attributes),
    insert_triple(Subject, Relation, Object, Attributes),
    writeln('Triple inserted with attributes.'),
    menu.

process_triple_without_attributes(Subject, Relation, Object) :-
    insert_triple(Subject, Relation, Object),
    writeln('Triple inserted without attributes.'),
    menu.
process_option(5) :-
    writeln('Enter the concept name:'),
    read(Concept),
    count_attributes(Concept, Count),
    format('Concept ~w has ~w attribute(s).~n', [Concept, Count]),
    menu.
process_option(6) :-
    generate_ontology_graph,
    writeln('DOT code generated and saved to "graph.txt".'),
    menu.
process_option(7) :-
    list_concepts,
    menu.
process_option(8) :-
    list_individuals,
    menu.
process_option(9) :-
    list_relations,
    menu.
process_option(10) :-
    list_triples,
    menu.
process_option(11) :-
    validate_existing_triples,
    menu.
process_option(0) :-
    writeln('Exiting...').

% Initialization of our program
:- initialization(init).
