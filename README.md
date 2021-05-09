# Genealogical Repositories

*OCaml, fully functional programming project for the Languages and Programming Environments class.*
*In this project we developed functions to process and make interisting queries in a special type*
*of data structure, called a Genealogical Gepository.*
*A repository is a 	list of pairs, each consisting on the identification of an individual, 
and the list of known children of the specific individual.*

# Structural Restrictions

_**Structural restrictions:**_
 * There are no duplicates among the first components of the various pairs.
 * All the known individuals occur in the first component.

_**Semantic restrictions:**_
 * There are no loops in the structure, no one can be an ancestor of itself.
 * Each individual can have at most two parents.

# Auxiliary Data Structures

*Some of the auxiliary data structures used in the project are Binary Trees, Lists and NNary Trees.*
_We use Binary Trees and NNary Trees respectively in the form of **Ancestor Trees** and **Descendants Trees**, the first storing
all the known Ancestors of a specific element, and the former all the know Descendants._

# Functions

- **height**: Determines the height of a given repository.
- **makeATree**: Makes an ancestor tree for a specific element, on a specific repository.
- **makeDTree**: Makes a descendants tree for a specific element, on a specific repository.
- **repOfATree**: Makes a repository data structure from an Ancestors Tree.
- **repOfDTree**: Makes a repository data structure from a Descendants Tree.
- **descendantsN**: Given a list of individuals, return all their descendants, "n" levels below.
- **siblings**: Given a list of individuals, return all their children.
- **siblingsInbreeding**: Find all pairs of siblings that have at least one child in common.
- **waveN**: Collect all the individuals that are at a specified distance from an element in the given list.
- **merge**: Merge two repositories.
- **supremum**: Given a list of individuals, return all their common ancestors.
- **validStructural**: Check the structural restrictions on a repository.
- **validSemantic**: Check the semantic restriction on a repository.
