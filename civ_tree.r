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

    # add class so we can use nicer print method
    class(tech) = c("civ2tech", class(tech))

    tech
    }


print.civ2tech = function(x){
    x = x[names(x) != "Required"]
    NextMethod(x)
    }


get_nodes = function(nodes, tech, where = c("Code", "Name")){
    if(is.numeric(nodes))
        return(nodes)
    where = match.arg(where)
    match(nodes, tech[[where]])
    }

get_node_parents = function(node, tech){
    if(is.character(node))
        node = get_nodes(node, tech)

    tech[node, c("Pr1", "Pr2")] |> as.character() |> na.rm()
    }


# rank of techs without prerequisites is 0
# rank of other nodes is max rank of prerequisites + 1
add_rank = function(tech){
    add_node_rank = function(node, tech){
        if(is.character(node))
            node = get_nodes(node, tech)
        if(is.na(tech[node, "Rank"])){
            parents = get_node_parents(node, tech) |> get_nodes(tech)
            for(parent in parents){
                tech = add_node_rank(parent, tech)
                }
            tech[node, "Rank"] = max(tech[parents, "Rank"]) + 1L
            }

        tech
        }

    tech$Rank = NA_integer_
    tech$Rank[is.na(tech$Pr1) & is.na(tech$Pr2)] = 0L

    for(node in tech$Code){
        tech = add_node_rank(node, tech)
        }

    tech
    }


na.rm = function(x) x[!is.na(x)]


add_required = function(tech){
    add_node_required = function(node, tech){
        if(is.character(node))
            node = get_nodes(node, tech)

        if(is.null(tech[node, "Required"][[1]])){
            parents = get_node_parents(node, tech)
            parents_pos = get_nodes(parents, tech)
            for(parent in parents){
                tech = add_node_required(parent, tech)
                }
            tmp = c(unlist(tech[parents_pos, "Required"]), parents) |> unique()
            tech[node, "Required"] = list(list(tmp))
            }

        tech
        }

    tech$Required = lapply(1:nrow(tech), \(x) NULL)
    tech$Required[is.na(tech$Pr1) & is.na(tech$Pr2)] = lapply(
        tech$Required[is.na(tech$Pr1) & is.na(tech$Pr2)], \(x) list()
        )


    for(node in tech$Code){
        tech = add_node_required(node, tech)
        }

    tech
    }


get_required = function(x, tech, translate = TRUE){
    node = c(get_nodes(x, tech, "Code"), get_nodes(x, tech, "Name")) |> na.rm()

    required = tech[node, "Required"][[1]]
    if(translate)
        required = tech[get_nodes(required, tech), "Name"]

    required
    }

add_count = function(tech){
    if(!hasName(tech, "Required"))
        tech = add_required(tech)

    tech$Count = sapply(tech$Required, \(x) x |> unique() |> length())
    tech
    }


tech = parse_rules()
tech = add_rank(tech)
tech = add_required(tech)
tech = add_count(tech)
tech = tech[order(tech$Rank, tech$Count),]
rownames(tech) = NULL

