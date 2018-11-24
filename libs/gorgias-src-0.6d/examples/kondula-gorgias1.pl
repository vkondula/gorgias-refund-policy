
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

:- compile('../lib/gorgias').
:- compile('../ext/lpwnf').

% Complements

complement(reject(R), entitled(R)).

% Rejection reason

rule(r1_default(R), reject(R), []).
rule(r2_customer_caused(R), reject(R), [caused(R)]).
rule(r3_warranty_expired(R), reject(R), [neg(warranty(R))]).

% Approved conditions

rule(r4_entilted(R), entitled(R), [neg(caused(R)), warranty(R)]).

% Approved results

rule(r4_1_default(R), refund(R), [entitled(R)]).
rule(r4_2_repairable(R), repair(R), [entitled(R), repairable(R)]).
rule(r4_2_selling(R), new(R), [entitled(R), selling(R)]).

% Preferences

rule(p1_entilted(R), prefer(r4_entilted(R), r1_default(R)), []).
rule(p2_repair(R), prefer(r4_2_selling(R), r4_1_default(R)), []).

% Repair Preferences

rule(p3_1_repair(R), prefer(r4_2_repairable(R), r4_1_default(R)), [repair_cheap(R)]).
rule(p3_2_repair(R), prefer(r4_2_repairable(R), r4_2_selling(R)), [repair_cheap(R)]).
rule(p3_3_repair(R), prefer(r4_1_default(R), r4_2_repairable(R)), []).
rule(p3_4_repair(R), prefer(r4_2_selling(R), r4_2_repairable(R)), []).

rule(pp_1_repair(R), prefer(p3_1_repair(R), p3_3_repair(R)), [repair_cheap(R)]).
rule(pp_2_repair(R), prefer(p3_2_repair(R), p3_4_repair(R)), [repair_cheap(R)]).

% Scenarios

rule(f_1_not_warranty, neg(warranty(r1)), []).

rule(f_2_caused, caused(r2), []).

rule(f_3_not_caused, neg(caused(r3)), []).
rule(f_3_warranty, warranty(r3), []).
rule(f_3_repairable, repairable(r3), []).
rule(f_3_repair_cheap, repair_cheap(r3), []).

rule(f_4_not_caused, neg(caused(r4)), []).
rule(f_4_warranty, warranty(r4), []).
rule(f_4_not_repairable, neg(repairable(r4)), []).

rule(f_5_not_caused, neg(caused(r5)), []).
rule(f_5_warranty, warranty(r5), []).
rule(f_5_not_repairable, neg(repairable(r5)), []).
rule(f_5_selling, selling(r5), []).

rule(f_6_not_caused, neg(caused(r6)), []).
rule(f_6_warranty, warranty(r6), []).
rule(f_6_repairable, repairable(r6), []).
rule(f_6_repair_cheap, repair_cheap(r6), []).
