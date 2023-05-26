:- dynamic ontologia/1, conceito/2, individuo/1, relacao/1, triplo/4.

% Insert the specification of a new concept
insert_concept_specification(Concept, Attributes) :-
    assertz(conceito(name(Concept), atribs(Attributes))).

% Insert a new individual
insert_individual(Individual) :-
    assertz(individuo(Individual)).

% Insert a new relation
insert_relation(Relation) :-
    assertz(relacao(Relation)).

% Insert a new triple
insert_triple(Subject, Relation, Object) :-
    assertz(triplo(Subject, Relation, Object, [])).

% Count the number of attributes of a concept
count_attributes(Concept, Count) :-
    conceito(name(Concept), atribs(Attributes)),
    length(Attributes, Count).

generate_dot_code :-
    tell('ontology_graph.dot'),
    writeln('digraph map {'),
    write_concept_nodes,
    write_relation_nodes,
    write_triples,
    writeln('}'),
    told.

write_concept_nodes :-
    conceito(name(Concept), _),
    format('  "~w" [shape=ellipse, style=filled, color=turquoise4];~n', [Concept]),
    format('  "name" [shape=rectangle, color=turquoise4];~n'),
    format('  "~w"->"name" [label="properties", style=dotted, color=red];~n', [Concept]),
    fail.
write_concept_nodes.

write_relation_nodes :-
    relacao(Relation),
    format('  "~w" [shape=ellipse, style=filled, color=turquoise4];~n', [Relation]),
    format('  "name" [shape=rectangle, color=turquoise4];~n'),
    format('  "~w"->"name" [label="properties", style=dotted, color=red];~n', [Relation]),
    fail.
write_relation_nodes.

write_triples :-
    triplo(Subject, Relation, Object, atribs(Attributes)),
    format('  "~w"->"~w" [label="~w", style=dashed];~n', [Subject, Object, Relation]),
    format('  "name=’~w’" [shape=rectangle, color=goldenrod];~n', [Subject]),
    format('  "~w"->"name=’~w’" [label="properties", style=dotted, color=red];~n', [Subject, Subject]),
    format('  "name=’~w’" [shape=rectangle, color=goldenrod];~n', [Object]),
    format('  "~w"->"name=’~w’" [label="properties", style=dotted, color=red];~n', [Object, Object]),
    fail.
write_triples.


% List all concepts
list_concepts :-
    writeln('Concepts:'),
    conceito(name(Concept), _),
    writeln(Concept),
    fail.
list_concepts.

% List all individuals
list_individuals :-
    writeln('Individuals:'),
    findall(Individual, individuo(Individual), Individuals),
    writeln(Individuals).

% List all relations
list_relations :-
    writeln('Relations:'),
    relacao(Relation),
    writeln(Relation),
    fail.
list_relations.

% List all triples
list_triples :-
    writeln('Triples:'),
    triplo(Subject, Relation, Object, _),
    format('Subject: ~w, Relation: ~w, Object: ~w~n', [Subject, Relation, Object]),
    fail.
list_triples.

% Validate existing triples and categorize them as valid or invalid
validate_existing_triples :-
    writeln('Valid Triples:'),
    validate_valid_triples,
    writeln('Invalid Triples:'),
    validate_invalid_triples.

% Validate valid triples
validate_valid_triples :-
    triplo(Subject, Relation, Object, _),
    validate_triple(Subject, Relation, Object),
    format('Subject: ~w, Relation: ~w, Object: ~w~n', [Subject, Relation, Object]),
    fail.
validate_valid_triples.

% Validate invalid triples
validate_invalid_triples :-
    triplo(Subject, Relation, Object, _),
    \+ validate_triple(Subject, Relation, Object),
    format('Subject: ~w, Relation: ~w, Object: ~w~n', [Subject, Relation, Object]),
    fail.
validate_invalid_triples.

% Validate a triple
validate_triple(Subject, Relation, Object) :-
    (
        (individuo(Subject); (conceito(name(Subject), _), Subject \= name(_))),
        relacao(Relation),
        (individuo(Object); (conceito(name(Object), _), Object \= name(_)))
    ).


% Initial ontology data
initialize_ontology_data :-
    % Insert concept specifications
    insert_concept_specification(name(city), [(name, string)]),
    insert_concept_specification(name(traveler), [(name, string)]),

    % Insert individuals
    insert_individual(pl),
    insert_individual(ine),
    insert_individual(pt),
    insert_individual(lx),

    % Insert relations
    insert_relation(alreadyVisited),
    insert_relation(lives),

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

% Read user's option
read_option :-
    writeln('Enter your option:'),
    read(Option),
    process_option(Option).

% Process user's option
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
    insert_triple(Subject, Relation, Object),
    writeln('Triple inserted.'),
    menu.
process_option(5) :-
    writeln('Enter the concept name:'),
    read(Concept),
    count_attributes(Concept, Count),
    format('Number of attributes: ~w~n', [Count]),
    menu.
process_option(6) :-
    generate_dot_code,
    writeln('DOT code generated.'),
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

% Run the program
:- initialization(init).
