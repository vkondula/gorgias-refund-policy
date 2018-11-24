
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%           Seller Policy, from KM papers                    %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Normally, sell a product at its high price.
% You can sell a product at the lower price only if paymentis cash.
% But normally prefer to sell high.
% Regular customers can be offered the low price (provided that they buy 2).
% (Also the low price can be offered if the buyer can have late delivery.)
% In high season you must sell at high prices.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- compile('../lib/gorgias').
:- compile('../ext/lpwnf').

% OPTIONS and complementarity

complement(sell(Prd,Ag,high), sell(Prd,Ag,low)).
complement(sell(Prd,Ag,low), sell(Prd,Ag,high)).

% Arguments for Options

rule(d1(Ag,Prd), sell(Prd,Ag,high), []).
rule(d2(Ag,Prd), sell(Prd,Ag,low), [pay_cash(Ag,Prd)]).

% Normal Policy ("Sell high")

rule(pr1(Ag,Prd), prefer(d1(Ag,Prd),d2(Ag,Prd)), []).

% Special Policy (Sell low to Regular Customers or Late Delivery)

rule(pr2(Ag,Prd), prefer(d2(Ag,Prd),d1(Ag,Prd)), [regular(Ag)]).

%rule(pr3(Ag,Prd), prefer(d2(Ag,Prd),d1(Ag,Prd)), [late_delivery(Ag,Prd)]).

% Special policy prevails/dominates (i.e. overrides the Normal policy)

rule(h_pr1(Ag,Prd), prefer(pr2(Ag,Prd), pr1(Ag,Prd)), []). %neg(high_season)

% Special policy does not apply in "high-season".
% In high-season normal policy prevails/dominates.

rule(h_pr2(Ag,Prd), prefer(pr1(Ag,Prd), pr2(Ag,Prd)), [high_season]).

% Fixed SCENARIA 

rule(f1, high_season, []).

% Hypothetical SCENARIA 

abducible(pay_cash(_,_),         []).

abducible(regular(_),            []).
%abducible(neg(regular(_)),      []).

