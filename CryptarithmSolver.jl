
# Get the set of all letters involved in the sum 
inwords = ["NORTH","SOUTH","EAST","WEST"];
outword = "EARTH";
allwords = vcat(inwords, outword);
characters = unique(join(allwords,""));

# For each letter I want a domain.
my_set = Set{Int32}([0,1,2,3,4,5,6,7,8,9]);
domains = Dict(c => deepcopy(my_set) for c in characters);

# we have a constraint that the first character of each word cannot be assigned to the digit 0
# need to get the first letter of each word
for word in allwords
    char = word[1]; # should be a char?
    curr_domain = domains[char]; # should get the current domain of this character
    delete!(curr_domain, 0); # I think this should delete the actual domain in the domains dict?
end

# we need a helper function to edit a copy of the domains given some assumption
# for example, if we know that letter D is an 8, then all other letters are not 8
# NB - in the recursion, remember that this modifies the original domain dict, so only call it on copies!
function update!(dom::Dict{Char, Set{Int32}}, char::Char, val::Int32)::Dict{Char, Set{Int32}}
    for k in keys(dom)
        if k == char
            dom[k] = intersect(dom[k],Set(val));
        else
            dom[k] = delete!(dom[k], val); # note this seems to work even if the key was not present, which is nice
        end
    end
    dom
end

# uncomment below to test
#=
test_dom = deepcopy(domains)
update!(test_dom, 'A',Int32(7))
=#

# Returns false if any letter has an empty domain
function check(dom::Dict{Char, Set{Int32}})::Bool
    for k in keys(dom)
        if length(dom[k]) == 0
            return false;
        end
    end
    return true;
end
    

# helper function to solve the problem for a single column
# given a set in in chars, the out char, and the remainder pased from the previous column, provide a set of valid domain assignments
function hypothesise(dom, inchars, outchar, rem)
    valid =  Set{Tuple{Dict{Char,Set{Int32}}, Int32}}();
    if length(inchars) == 0
        # then we only need to set the outchar to be equal to the remainder, modulo 10
        # check if this is possible
        ideal = convert(Int32,mod(rem, 10));
        if ∈(ideal, dom[outchar])
            # then we can make this work!
            new_dom = deepcopy(dom);
            update!(new_dom, outchar, ideal);
            push!(valid,(new_dom, convert(Int32, rem - ideal))); # remember to divide rem by 10 when passing forward!
        end
        return valid;
    else
        # we still have some inchars we could fix
        # pick the first inchar and find all values it can take
        possible = dom[inchars[1]];
        for p in possible
            new_dom = deepcopy(dom);
            update!(new_dom, inchars[1], p);
            if !check(new_dom)
                continue
            end
            new_inchars = deepcopy(inchars[2:end]);
            new_rem = convert(Int32,rem + p);
            solns = hypothesise(new_dom, new_inchars, outchar, new_rem);
            if length(solns) > 0
                # then the downstream process found some valid hypotheses!
                for (s_dom, s_rem) in solns # solns should be a set of tuples
                    #s_dom[inchars[1]] = Set(p)
                    push!(valid, (s_dom, s_rem));
                end
            end
        end
    end
    return valid;
end

#test
#hypothesise(domains, ['H','H','T','T'], 'H',0)

function droplast(B)
    B[1:(end-1)];
end
function getlast(B)
    if !isempty(B)
        return last(B);
    end
end
function getlasts(B)
    res = Array{Char,1}();
    for i in B
        if !isempty(i)
            push!(res,last(i));
        end
    end
    return res;
end

# now for the main event
#= If the inwords have no more characters left, but the outword does have left, 
then the remainder must match the remaining characters of the outword exactly.
If such a match can be found, return it, otherwise return an empty set.
If the inwords do have characters left, then get the next column of characters.
Generate all hypotheses that are valid given that column.
If there are no valid hypotheses, return an empty set.
Otherwise, for each Hypothesis, create the implied domain, and pass in the words all missing their last letter to the recursion
For each hypothesis we get back a set.  If it is non-empty, add it to our set of valid solutions
return the set of valid solutions
=#
function solve(dom::Dict{Char,Set{Int32}}, inwords, outword, remainder)
    solutions = Set{Dict{Char,Set{Int32}}}();
    
    if !any(length.(inwords) .> 0)
        
        if length(outword) > 0
            # then we need the remainder to match the outword digits exactly
            # first check if we have the right number of digits
            
            if length(outword) == length(digits(remainder))
                # if the number of digits is right we should be able to just start assigning from the right
                ideal = mod(remainder,10);
                new_dom = deepcopy(dom);
                update!(new_dom, outword[end], ideal);
                
                if check(new_dom)
                                   
                    if length(outword) > 1
                        new_outword = outword[1:(end-1)];
                        new_remainder = convert(Int32,(remainder - ideal)/10);
                        soln = solve(new_dom, 
                                     inwords, 
                                     new_outword, 
                                     new_remainder); # should be a set of domains which are valid solutions
                        push!(solutions,soln);
                    else
                        push!(solutions,new_dom);
                    end
                end
            end
        else
            # now we're in the case where both inwords and outwords are finished
            
            if remainder == 0
                push!(solutions, dom); # this must be valid
            end
        end
    else
        #now we're in the case where there's at least one inword with at least 1 character left
        inchars = getlasts(inwords);
        outchar = getlast(outword);
        hyp = hypothesise(dom, inchars, outchar, remainder);    
        if length(hyp) > 0
            # we might still be in business
            new_inwords = droplast.(inwords);
            new_outword = droplast(outword);
            for (h_dom, h_rem) in hyp
                soln = solve(h_dom, new_inwords, new_outword, convert(Int32, h_rem/10)); # dividing by 10 since we're moving left
                if length(soln) > 0    
                    for s in soln
                        push!(solutions, s);
                    end
                end
            end            
        end
    end
    return solutions;
end

println("Solve time including compile time")
@time solution = solve(domains,inwords, outword, 0);

# report

function report(dom)
    l = maximum(length.(vcat(inwords, outword)))
    println("\nFound a solution: ")
    for word ∈ vcat(inwords, outword)
        my_num = ""
        for char ∈ word
            digit = getindex(collect(dom[char]),1)
            my_num = string(my_num, digit)
        end
        println(lpad(word,l), ": ", lpad(my_num,l))
    end
end

#my_dom = getindex(collect(solution),1);
#report(my_dom);
report.(solution);

println("Solve time excluding compile time")
@time solution = solve(domains,inwords, outword, 0);
