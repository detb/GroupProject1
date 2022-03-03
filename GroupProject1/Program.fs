// PAYMENT AND PRODUCT
type PaymentMethod = CreditCard | Cash | MobilePay
type Product = Drink | Food | Fruit

// DEFINING CUSTOMER AND ORDER 
type Customer = VIAPerson | SOSUPerson | Guest

type Order = {
    Products : list<Product>
    PaymentMethods : list<PaymentMethod>
}    

type VIAPerson = {
    FirstName : string
    LastName : string
    Age : int
    Id : string
    Orders : list<Order>
}

type SUSOPerson = {
    FirstName : string
    LastName : string
    Age : int
    Id : string
    Orders : list<Order>
}

// DEFINITION OF DRINKS
type Size = Large | Medium | Small

type Coffee = Black | WithMilk | WithMilkAndSugar
type Juice = Orange | Apple | GingerLemon
type Tea = Earl_gray | Dar_jeeling | StrawberryRhubarb

type Category = Tea | Coffee | Juice // CREATE RECORDS FOR EACH TYPE INSTEAD

type Drink = {
    SizeTemp : Size
    CategoryTemp : Category
}

type Drink2  {
    CoffeeF of Coffee 
    | F of Juice
}


let calculateCategory (d:Drink) = 
    match d.CategoryTemp with 
    | Tea -> 0
    | Coffee -> 5
    | Juice -> 10

let calculateSize (d:Drink) = 
    match d.SizeTemp with
    | Large -> 7
    | Medium -> 5
    | Small -> 0

let drinkPrice (d:Drink) = 
   (calculateCategory d) + (calculateSize d) + 10 

//type OrderProduct = Product * int

let p1 = Product(Drink = ({SizeTemp = Large; CategoryTemp = Tea(Earl_gray)}))
let Product(Drink) drink2 = {SizeTemp = Medium; CategoryTemp = Juice}


//let p = Product(Drink{Category{}, Size{})

let  testPerson = Customer(VIAPerson{FirstName="William";LastName="Test";Age=22;Id="283109";Orders=order})

let order = {
    Products = [drink1;drink2]; PaymentMethods = [CreditCard;MobilePay]; 
}