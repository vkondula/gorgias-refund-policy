
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                      Refund Policy                         %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Normally, reject refund requests to save money.
% If the buyer caused the damage, also reject the request.
% Do not accept items that are out of warranty period or
% do not have live long warranty.
% If it’s technical issue, try to repair the item,
% but only if it won’t cost more than buying a new one.
% In that case exchange for a new item.
% If the item is no longer available and customer
% should be compensated, return money.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- compile('gorgias-src-0.6d/lib/gorgias').
:- compile('gorgias-src-0.6d/ext/lpwnf').

:- dynamic considered/1, repair_price/2, caused/1, selling/1, exists/1, price/2, bought_days_ago/2, lifelong_warranty/1.

% Complements

complement(reject(R), entitled(R)).

% Rejection reason

rule(r0_non_existing(R), reject(R), [neg(exists_g(R))]).
% rule(r1_default(R), reject(R), []).
rule(r2_customer_caused(R), reject(R), [caused_g(R)]).
rule(r3_warranty_expired(R), reject(R), [neg(warranty_g(R))]).

% Approved conditions

rule(r4_entilted(R), entitled(R), [neg(caused_g(R)), warranty_g(R)]).

% Approved results

rule(r4_1_default(R), refund(R), [entitled(R)]).
rule(r4_2_repairable(R), repair(R), [entitled(R), repair_cheap_g(R)]).
rule(r4_2_selling(R), exchange(R), [entitled(R), selling_g(R)]).

% Preferences

rule(p1_entilted(R), prefer(r4_entilted(R), r1_default(R)), []).
rule(p2_repair(R), prefer(r4_2_selling(R), r4_1_default(R)), []).

% Repair Preferences

rule(p3_1_repair(R), prefer(r4_2_repairable(R), r4_1_default(R)), [repair_cheap_g(R)]).
rule(p3_2_repair(R), prefer(r4_2_repairable(R), r4_2_selling(R)), [repair_cheap_g(R)]).
rule(p3_3_repair(R), prefer(r4_1_default(R), r4_2_repairable(R)), []).
rule(p3_4_repair(R), prefer(r4_2_selling(R), r4_2_repairable(R)), []).

rule(pp_1_repair(R), prefer(p3_1_repair(R), p3_3_repair(R)), [repair_cheap_g(R)]).
rule(pp_2_repair(R), prefer(p3_2_repair(R), p3_4_repair(R)), [repair_cheap_g(R)]).

% Fact translation

rule(f_caused, caused_g(R), []):- considered(R), caused(R).
rule(f_warranty, warranty_g(R), []):- considered(R), warranty(R).
rule(f_repairable, repairable_g(R), []):- considered(R), repairable(R).
rule(f_repairable, repair_cheap_g(R), []):- considered(R), repair_cheap(R).
rule(f_selling, selling_g(R), []):- considered(R), selling(R).
rule(f_exists, exists_g(R), []):- considered(R), exists(R).

rule(f_not_caused, neg(caused_g(R)), []):- exists(R), \+ caused(R).
rule(f_not_warranty, neg(warranty_g(R)), []):- exists(R), \+ warranty(R).
rule(f_not_repairable, neg(repairable_g(R)), []):- exists(R), \+ repairable(R).
rule(f_not_repairable, neg(repair_cheap_g(R)), []):- exists(R), \+ repair_cheap(R).
rule(f_not_selling, neg(selling_g(R)), []):- exists(R), \+ selling(R).
rule(f_not_exists, neg(exists_g(R)), []):- considered(R), \+ exists(R).

% Common sense logic

warranty(R):- bought_days_ago(R, X), X < 713, !.
warranty(R):- lifelong_warranty(R), !.

repair_cheap(R):- repair_price(R, X), price(R, Y), X < Y.

repairable(R):- repair_cheap(R).

caused(placeholder).
