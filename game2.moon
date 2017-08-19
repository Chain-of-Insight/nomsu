#!/usr/bin/env moon
Game = require 'nomic2'
core_game = require 'core2'

game = Game(core_game)

game\def "# $key $relation = $value", (args)=>
    --print "Setting #{args.key} #{args.relation} = #{args.value}"
    @relations[args.relation][args.key] = args.value
    return args.value

game\def "set all * $relation = $value", (args)=>
    rels = @relations[args.relation]
    for k,_ in pairs rels
        rels[k] = args.value
    if next(rels) == nil
        @relations[args.relation] = nil
    return args.value

game\def "$key $relation ?", (args)=>
    return @relations[args.relation][args.key]

deep_pairs = (t)->
    coroutine.wrap ->
        for k,v in pairs(t)
            coroutine.yield k, v
        mt = getmetatable(t)
        return unless mt and mt.__index and type(mt.__index) == 'table' and mt.__index != t
        for k,v in deep_pairs mt.__index
            if t[k] == nil
                coroutine.yield k,v

game\def "* $relation = $value", (args)=>
    --print "Finding all * #{args.relation} = #{args.value}"
    [key for key, value in deep_pairs(@relations[args.relation]) when value == args.value]

game\def {"restrict $actions to $whitelist"}, (args)=>
    with args
        actions = @canonicalize(if type(.actions) == 'table' then .actions else {.actions})
        whitelist = @all_aliases(if type(.whitelist) == 'table' then .whitelist else {.whitelist})
        @\set_whitelist actions, whitelist
        for action in *actions
            print("Restricting #{Game.repr(action)} to #{Game.repr(whitelist)}")

game\def {"permit $whitelist to $actions"}, (args)=>
    with args
        actions = @canonicalize(if type(.actions) == 'table' then .actions else {.actions})
        whitelist = @all_aliases(if type(.whitelist) == 'table' then .whitelist else {.whitelist})
        for action in *actions
            if not @authorized[action]
                print "#{action} is already available to everyone."
                continue
            print("Permitting #{Game.repr(action)} to #{Game.repr(whitelist)}")
            for w in *whitelist
                @authorized[action][w] = true

game\def {"revoke $actions rights from $whitelist"}, (args)=>
    with args
        actions = @canonicalize(if type(.actions) == 'table' then .actions else {.actions})
        whitelist = @all_aliases(if type(.whitelist) == 'table' then .whitelist else {.whitelist})
        for action in *actions
            if not @authorized[action]
                print "#{action} is available to everyone, it can't be restricted."
                continue
            print("Revoking the right of #{Game.repr(action)} to use #{Game.repr(whitelist)}")
            for w in *whitelist
                @authorized[action][w] = nil

game\def "print callstack", (args)=>
    print("Callstack:")
    for fn in *@callstack
        print fn

game\def {"do $action"}, (args)=>
    (args.action)(@, {})

game\def {"make $who $action"}, (args)=>
    with args
        old_you = @you
        print("Setting you=#{.who}")
        rawset(@, "you", .who)
        (.action)(@, {})
        rawset(@, "you", old_you)

game\def "you", (args)=> @you

game\run[=[
say "===================================================="
say "                    NEW GAME"
say "===================================================="
"everyone approves $action" := {yes}

"sudo $action" := {
    if (everyone approves $action) {
        do $action
    } else {
        say "You do not have the will of the people! >:("
    }
}
restrict "$ := $" to "sudo $"
restrict "make $ $" to "sudo $"
restrict "set all * $ = $" to "sudo $"
restrict "# $ $ = $" to "sudo $"
restrict "restrict $ to $" to "sudo $"

sudo {
    "propose $action" := {
        if ("pending proposal" "is" ?) {
            say "Sorry, an action is already pending."
        } else {
            say "Proposing..."
            # "pending proposal" "is" = $action
        }
    }
    "unpropose" := {
        let "pending" = ("pending proposal" "is" ?)
        set all * $pending = (nil)
        # "pending proposal" "is" = (nil)
    }


    "mark $who as approving $action" := {
        # $who $action = "approved"
    }

    "mark $who as rejecting $action" := {
        # $who $action = "rejected"
    }

    ["approve", "vote yes", "vote yea"] := {
        let "pending" = ("pending proposal" "is" ?)
        mark (you) as approving $pending
        say "Voted yes."
        if (everyone approves $pending) {
            sudo $pending
            unpropose
        }
    }

    ["reject", "vote no", "vote nay", "veto", "disapprove"] := {
        let "pending" = ("pending proposal" "is" ?)
        mark (you) as rejecting $pending
        say "Voted no."
        unpropose
    }

    ["players", "everyone", "everybody", "all players"] := {
        * "is a player" = (yes)
    }

    "join" := {
        # (you) "is a player" = (yes)
        printf ["Welcome to the game, ",(you),"!"]
    }
    permit "unpropose" to "set all * $ = $"
    permit ["join", "mark $ as approving $", "mark $ as rejecting $", "propose $", "unpropose"] to "# $ $ = $" 
    restrict "unpropose" to ["approve", "reject"]
    restrict "mark $ as approving $" to ["propose $", "approve"]
    restrict "mark $ as rejecting $" to ["propose $", "reject"]

    "everyone approves $action" := {
        (# (players)) == (# (* $action = "approved"))
    }
}

join

propose {
    say "fart"
}
approve


"cheat" := {
    say "CHEATER!!!"
}
sudo {
    say "CHEATER!!!"
}

propose {
    # "democracy" "is possible" = (yes)
    if ("democracy" "is possible" ?) {
        say "DEMOCRACY WORKS!!!"
    }
}
approve

propose {
    "fart" := {
        say "poot"
    }
}
approve
fart

propose {
    "open election $candidates" := {
        if ("candidates" "are" ?) {
            say "An election is already in progress."
        } else {
            # "candidates" "are" = $candidates
        }
    }

    "close election" := {
        let "pending" = ("pending proposal" "is" ?)
        set all * "votes for" = (nil)
        # "candidates" "are" = (nil)
    }

    "vote for $candidate" := {
        # (you) "votes for" = $candidate
        let "vote-percent" = ((# (* "votes for" = $candidate)) / (# (players)))
        printf ["Vote cast. ",$candidate," now has ",(100 * $vote-percent),"% of the votes."]
        if ($vote-percent > 0.5) {
            printf ["The winner of the election is:", $candidate]
            close election
        }
    }

    permit ["open election $", "close election", "vote for $"] to ["# $ $ = $"]
    permit ["close election"] to ["set all * $ = $"]
}
approve

propose {
    "as bill: $action" := {
        if ((you) == "Anonymous") {
            make "bill" $action
        } else {
            printf ["Who do you think you are?", (you)]
        }
    }
    permit ["as bill: $"] to ["make $ $"]
}
approve
as bill: {join}

propose {
    "as dave: $action" := {
        if ((you) == "Anonymous") {
            make "dave" $action
        } else {
            printf ["Who do you think you are?", (you)]
        }
    }
    permit ["as dave: $"] to ["make $ $"]
}
approve
as bill: {approve}
as dave: {join}

open election ["tom", "dick", "harry"]
vote for "dick"
as bill: {vote for "dick"}

propose {
    "take a shit" := {say "shit taken."}
}
approve
as bill: {approve}
as dave: {approve}



sudo {
    "everyone approves" := {
        (# (players)) == (# (* "votes" (yes)))
    }
    ["approve", "vote yes", "vote yea"] := {
        # (you) "votes" = (yes)
        if (everyone approves) {
            do pending action
        }
    }
    ["disapprove", "vote no", "vote nay", "veto"] := {
        say "The proposal has failed."
        # (you) "approves" = (yes)
        if (everyone approves) {
            do pending action
        }
    }
}

sudo {
    "everyone approves" := {
        say "Going into this code"
        no
    }
}
sudo {
    say "BROKEN"
}


propose {
    "arbitrarily define $signature := $body" := {
        if ($signature == "butts") {
            $signature := $body
        } else {
            say "Not my style."
        }
    }
    permit "arbitrarily define $ := $" to "$ := $"
    say "Arbitrary is a go."
}
as bill: {approve}
as dave: {approve}
approve
arbitrarily define "butts" := {say "BUTTS"}
butts

arbitrarily define "ass" := {say "ASS"}
ass

]=]
