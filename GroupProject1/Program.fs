//Payments:
type PaymentType = Creditcard | Cash | Mobilepay
type Payment = {
    Type: PaymentType
    Weight: double
}

//Products:
type drinkTypes = Milk | Coffee | Tea
type fruitTypes = Banana | Strawberry | Watermelon
type foodTypes = Meatballs | Pizza | TBoneSteak

//Product type and sizes:
type ProductTypes = Drink of drinkTypes  | Fruit of fruitTypes | Food of foodTypes
type ProductSizes = Small | Medium | Large
type Product = {
     Type: ProductTypes
     ProdSize: ProductSizes
}

//Price calcs.
let calcProductPrice (prod:Product) =
    match prod.Type, prod.ProdSize with
    | Drink Milk, Small -> 15
    | Drink Milk, Medium -> 20
    | Drink Milk, Large -> 25
    | Drink Tea, Small -> 5
    | Drink Tea, Medium -> 10
    | Drink Tea, Large -> 15
    | Drink Coffee, Small -> 20
    | Drink Coffee, Medium -> 25
    | Drink Coffee, Large -> 30
    | Food Meatballs, Small -> 30
    | Food Meatballs, Medium -> 45
    | Food Meatballs, Large -> 50
    | Food Pizza, Small -> 30
    | Food Pizza, Medium -> 45
    | Food Pizza, Large -> 50
    | Food TBoneSteak, Small -> 30
    | Food TBoneSteak, Medium -> 45
    | Food TBoneSteak, Large -> 50
    | Fruit Banana, _ -> 15
    | Fruit Strawberry, _ -> 20
    | Fruit Watermelon, _ -> 25

//Orders
type OrderSpec = {
    ProdList: List<Product>
    mutable price: int
    PaymentMethods: List<Payment>
}

//Fun calc price of order
let orderCalc (item:OrderSpec) =
    let priceList = item.ProdList |> List.map calcProductPrice
    item.price <- List.sum priceList

//dette kan bruges måske
type StudyHouse = VIA | SOSU
type Customer = {
     First: string
     Last: string
     School: StudyHouse
     mutable Orders: List<OrderSpec>
     StudyNumber: int
}

//function "Add to List" (add order to mutable list)
let addToList (cust:Customer) (item:OrderSpec) = 
    cust.Orders <- item :: cust.Orders

//Test data.

//Products:
let prodTest = {Type = Drink Milk; ProdSize=Small}
let prodTest2 = {Type = Drink Coffee; ProdSize=Large}

let productStrawberry = {Type = Fruit Strawberry; ProdSize=Small}
let productBanana = {Type = Fruit Banana; ProdSize=Small}
let productTBone = {Type = Food TBoneSteak; ProdSize=Large}
let productPizza = {Type = Food Pizza; ProdSize=Large}

//PaymentMethods:
let pay50Mobile = {Type = Mobilepay; Weight = 0.5}
let pay100Mobile = {Type = Mobilepay; Weight = 1}
let pay50Cash = {Type = Cash; Weight = 0.5}
let pay100Cash = {Type = Cash; Weight = 1}

//Orders
let firstOrder = {ProdList = [prodTest; prodTest2]; price = 0; PaymentMethods = [pay50Mobile; pay50Cash]}
let secondOrder = {ProdList = [productBanana; prodTest2]; price = 0; PaymentMethods = [pay100Cash]}

//Customers
let cust1:Customer = { First = "Mat"; Last = "leth"; School = VIA; Orders = List.Empty; StudyNumber = 15 }
let teacher1:Customer = { First = "Lars"; Last = "Leth"; School = VIA; Orders = List.Empty; StudyNumber = 202423 }

//Add Order to customer method
orderCalc secondOrder

addToList cust1 firstOrder
addToList teacher1 secondOrder

//out prints (Ved ikke hvordan jeg printer "Type: StudyHouse"
printf "TEST CUST1 ----> Customer 1 = First: %s Last: %s Orders: %A Study House: %A StudyNr: %i" cust1.First cust1.Last cust1.Orders cust1.School cust1.StudyNumber 
printf "TEST Teacher order ----> Teacher 1 = First: %s Last: %s Orders: %A Study House: %A StudyNr: %i" teacher1.First teacher1.Last teacher1.Orders teacher1.School teacher1.StudyNumber 
