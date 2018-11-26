def translate(reasons: list, parameters: dict) -> list:
    retval = []
    if "f_not_caused" in reasons:
        retval.append("Customer didn't caused the damage.")
    if "f_warranty" in reasons:
        if parameters.get("lifelong_warranty"):
            retval.append("Item does have life long warranty.")
        else:
            retval.append("Item is still in warranty period.")
    if any(["r3_warranty_expired" in r for r in reasons]):
        retval.append("Item is already out of warranty period.")
    if "f_caused" in reasons:
        retval.append("Customer caused the damage on the item.")
    if "f_not_exists" in reasons:
        retval.append("Item with given serial number does not exist.")
    if any(["r4_2_repairable" in r for r in reasons]):
        retval.append("It's cheaper to repair the item then refund money.")
    if any(["r4_2_selling" in r for r in reasons]):
        if parameters.get("repair_price"):
            retval.append(
                "Item is still available for sale and "
                "it's cheaper to get new item then repair the original one."
            )
        else:
            retval.append(
                "Item is still available for sale and "
                "the repair price is hard to estimate."
            )
    if any(["r4_1_default" in r for r in reasons]):
        if parameters.get("repair_price"):
            retval.append(
                "It's too expensive to repair the item "
                "and it's not in stock anymore."
            )
        else:
            retval.append(
                "The repair price is hard to estimate and it's not in stock anymore."
            )
    return retval
