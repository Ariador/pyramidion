# The Pyramidion
Pyramidion Non-Relational Database, v.0.19 (alpha)

(c) 2018-2025 by Igor Voloshin ivoloshin@hotmail.com

Programming language: Free Pascal

IDE: Lasarus

Operation system: Ubuntu

This database models an oriented graph consisting of nodes connected with directed edges. Features are the following:
1. Both nodes and edges are stored as elements of the same array, due to reason an edge is essentially a node with one ascending (incoming) and one descending (outgoing) edge;
2. Duplicate and cyclic edges are forbidden here, so Pyramidion is a model of the oriented graph (orgraph) specifically, and not the directed graph;
3. Also orphaned (isolated) nodes are forbidden, thus making Pyramidion to be specifically a model of the connected orgraph. To prevent the orphaned nodes appearing each new node is automatically linked with the initial Node 0. It's by far the easiest way, allowing for productivity at the cost of memory space;
4. Types of edges introduced, developping the idea 'Edge is a Node'. These types are just references to nodes, representing edge types, so-called 'property nodes', this typisation mechanism is implemented just to increase the performance and to save storage space;
5. The latest version (still alpha) is v.0.19, archived here. Version 0.20 is now in development.

The Pyramidion DBMS is the base for development of the new Runar programming language which is a declarative esoteric language slightly resembling by the idea (NOT by the form) the Haskell language. Runar is based on the combinatory logic and the λ-calculus. And on Futhark runes.
I am now writing a manual for the Runar language and will put it here as the separate project, for easier maintainability.
