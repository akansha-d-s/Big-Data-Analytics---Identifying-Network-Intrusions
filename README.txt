These files have function definitions and set up and must be executed before anything else can be done:

Task1.R, Task2.R, Misclassification.R, Node.R, DecisionTree.R, DatabaseScanner.R, Connection.R

Connection.R includes a SQL query, so it will take some time to complete.

Make a Tree: makeTree(conection, attributes, impurity function, a conditional to use in the where portion of a sql query, the original indentation of the tree (empty string), maximum error, minimum number of elements)

Get Accuracy of Tree:verification(connection, table name, impurity calculation, (optional) reader structure)

Print a Tree:printTree(tree).For examples of these, refer to main.R

main.R loops through multiple error and minimum elements, creates trees, writes them into files, calculated the misclassification, then writes that to a log file.


