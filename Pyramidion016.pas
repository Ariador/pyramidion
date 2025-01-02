
//==================================================================================================================================================================================
//  PYRAMIDION NON-RELATIONAL DATABASE, v.0.16 (alpha version)
//----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
//  (c) 2021 Igor Voloshin ivoloshin@hotmail.com
//==================================================================================================================================================================================
//  This database models an oriented graph consisting of nodes connected with directed edges. Features are the following:
//  1. Both nodes and edges are stored as elements of the same array, due to reason an edge is essentially a node with one ascending (incoming) and one descending (outgoing) edge;
//  2. Duplicate and cyclic edges are forbidden here, so Pyramidion is a model of the oriented graph (orgraph) specifically, and not the directed graph;
//  3. Also orphaned (isolated) nodes are forbidden, thus making Pyramidion to be specifically a model of the connected orgraph (SPECIAL REMARK: to prevent the orphaned nodes
//     appearing each new node is automatically linked with the initial Node 0. It's by far the easiest way, allowing for productivity at the cost of memory space);
//  4. Types of edges introduced, developping the idea 'Edge is a Node'. These types are just references to nodes, representing edge types, so-called 'property nodes', this
//     typisation mechanism is implemented just to increase the performance and to save storage space.
//==================================================================================================================================================================================

program Pyramidion016;

{$mode objfpc}{$H+}
{$S+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, lazcontrols, Aesthesia016, Apostolia016, uecontrols;

{$R Pyramidion016.res}

begin
  RequireDerivedFormResource:=True;
  Application.Scaled:=True;
  Application.Initialize;
	  Application.Title:='Pyramidion 016';
  Application.CreateForm(TAesthesiaForm, Form);
  Application.Run;
end.

