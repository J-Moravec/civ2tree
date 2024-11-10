# civ_tree.r
#
# parsing civ2 tech tree

# Parse RULES.TXT and get tech tree in a data.frame
parse_rules = function(){
    rules = readLines("RULES.TXT")
    from = grep("@CIVILIZE", rules) + 1
    to = grep("^Future Technology", rules)
    tech = rules[from:to]
    # replace all whitespace with a single space
    tech = tech |> gsub(pattern = "[[:space:]]+", replace = " ")
    # replace ";" with "," so we have completely comma-separated fields
    tech = tech |> sub(pattern = ";", replace = ",", fixed = TRUE)
    # filter out unused techs
    # most of them are after Future Technology,
    # the only remaining one is Plumbing
    tech = tech |> grep(pattern = "no, no,", value = TRUE, invert = TRUE)

    # we could do strsplit and parse ourselves, but this way is a bit easier
    con = textConnection(tech)
    tech = read.csv(
        con,
        header = FALSE,
        strip.white = TRUE, # whitespace cleanup
        na.strings = "nil",  # nil -- no prerequisite
        )

    # pick columns of interest:
    # Name, Prerequisite 1 and 2, and Code
    tech = tech[c(1, 4:5, 8)] |> setNames(c("Name", "Pr1", "Pr2", "Code"))
    tech
    }


get_node = function(x, tech){
    which(x == tech$Code)
    }

get_node_parents = function(node, tech){
    c(
        which(tech[node, "Pr1"] == tech$Code),
        which(tech[node, "Pr2"] == tech$Code)
        )
    }

# rank of techs without prerequisites is 0
# rank of other nodes is max rank of prerequisites + 1
add_rank = function(tech){
    add_node_rank = function(node, tech){
        if(is.na(tech[node, "Rank"])){
            parents = get_node_parents(node, tech)
            for(parent in parents){
                tech = add_node_rank(parent, tech)
                }
            tech[node, "Rank"] = max(tech[parents, "Rank"]) + 1L
            }

        tech
        }

    tech$Rank = NA_integer_
    tech$Rank[is.na(tech$Pr1) & is.na(tech$Pr2)] = 0L

    for(i in seq_len(nrow(tech))){
        tech = add_node_rank(i, tech)
        }

    tech
    }


na.rm = function(x) x[!is.na(x)]

add_count = function(tech){
    add_node_count = function(node, tech){
        if(is.null(tech[node, "Count"][[1]])){
            parents = get_node_parents(node, tech)
            for(parent in parents){
                tech = add_node_count(parent, tech)
                }
            tmp = c(unlist(tech[parents, "Count"]), parents) |> unique()
            tech[node, "Count"] = list(list(tmp))
            }

        tech
        }

    tech$Count = lapply(1:nrow(tech), \(x) NULL)
    tech$Count[is.na(tech$Pr1) & is.na(tech$Pr2)] = lapply(
        tech$Count[is.na(tech$Pr1) & is.na(tech$Pr2)], \(x) list()
        )

    for(i in seq_len(nrow(tech))){
        tech = add_node_count(i, tech)
        }

    tech$Count = sapply(tech$Count, \(x) x |> unique() |> length())
    tech
    }


tech = parse_rules()
tech = add_rank(tech)
tech = add_count(tech)
tech = tech[order(tech$Rank, tech$Count),]
rownames(tech) = NULL

