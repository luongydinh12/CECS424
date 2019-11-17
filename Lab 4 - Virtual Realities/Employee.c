// Name: Dinh Luong
// Lab Assignment 4 : Virtual Realities
// CECS 424
// Professor Neal Terrell

#include <stdio.h>
#include <stdlib.h>

// #1.1  struct Employee
struct Employee {
    void** vtablePtr;
    int age;
};

// #1.2 struct HourlyEmployee
struct HourlyEmployee {
    void** vtablePtr;
    int age;
    double hourly_rate;
    double hours;
};

// #1.3 struct CommissionEmployee
struct CommissionEmployee {
    void** vtablePtr;
    int age;
    double sales_amount;
};

// #2 Global functions:
// #2.1 Speak_Hourly
void Speak_Hourly (struct Employee* empPtr){
    struct HourlyEmployee *hEmpPtr = (struct HourlyEmployee*) empPtr;
    printf("I am an Hourly Employee\n");
    printf("I work for $%.2f dollars per hour\n", hEmpPtr->hourly_rate);
};
// #2.2 Speak_Commission
void Speak_Commission (struct Employee* empPtr){
    struct CommissionEmployee *cEmpPtr = (struct CommissionEmployee*) empPtr;
    printf("I am a Commission Employee\n");
    printf("I make commission on $%.2f dollars in sales!\n", cEmpPtr->sales_amount);
};

// #3 GetPay:
// #3.1 GetPay_Hourly
double GetPay_Hourly(struct Employee* empPtr){
    struct HourlyEmployee *hEmpPtr = (struct HourlyEmployee*) empPtr;
    double totalPay = hEmpPtr->hours *hEmpPtr->hourly_rate;
    return totalPay;
};
// #3.2 GetPay_Commission
double GetPay_Commission(struct Employee* empPtr){
    struct CommissionEmployee *cEmpPtr = (struct CommissionEmployee*) empPtr;
    double totalPay = 0.1*cEmpPtr->sales_amount+40000;
    return totalPay;
};

// #4 Vtable_XX
// #4.1 Vtable_Hourly
void* Vtable_Hourly[2] = {Speak_Hourly, GetPay_Hourly};
// #4.2 Vtable_Commission
void* Vtable_Commission[2] = {Speak_Commission, GetPay_Commission};

// #5 Construct_XX
// #5.1 Construct_Hourly
void Construct_Hourly (struct HourlyEmployee* hEmpPtr){
    hEmpPtr->age = 0;
    hEmpPtr->hourly_rate = 0;
    hEmpPtr->hours = 0;
    hEmpPtr->vtablePtr = Vtable_Hourly;
};
// #5.2 Construct_Commission
void Construct_Commission (struct CommissionEmployee* cEmpPtr){
    cEmpPtr->age = 0;
    cEmpPtr->sales_amount = 0;
    cEmpPtr->vtablePtr = Vtable_Commission;
};

// #6 New class "SeniorSalesman"
struct SeniorSalesman {
    void** vtablePtr;
    int age;
    double sales_amount;
};
double GetPay_Senior(struct Employee* empPtr){
    struct SeniorSalesman *sEmpPtr = (struct SeniorSalesman*) empPtr;
    double extra = 0;
    if (sEmpPtr->age >= 40){
        extra = 0.05*sEmpPtr->sales_amount;
    }
    double totalPay = 0.2*sEmpPtr->sales_amount+ 50000 + extra;
    return totalPay;
};
void* Vtable_Senior[2] = {Speak_Commission, GetPay_Senior};
void Construct_Senior (struct SeniorSalesman* sEmpPtr){
    sEmpPtr->age = 0;
    sEmpPtr->sales_amount = 0;
    sEmpPtr->vtablePtr = Vtable_Senior;
};

void Option(){
    printf("Please select your options\n");
    printf("1. Hourly Employee\n");
    printf("2. Commission Employee\n");
    printf("3. Senior Salesman\n");
};

int Age(){
    printf("Input your age: ");
    int age = 0;
    scanf ("%d",&age);
    return age;
};

int main(){
    // #1 Declare Employee pointer    
    struct Employee *ePtr;
    struct HourlyEmployee *hEmp;
    struct CommissionEmployee *cEmp;
    struct SeniorSalesman *sEmp;

    // #2 User choice and #3 malloc
    int cont = 0;
    int choice = 0;
    int age = 0;
    double paid = 0;
    double amtSales = 0;
    double payRate = 0;
    double hours = 0;
    while (cont != -1 ){
        Option();
        scanf ("%d",&choice);

        // #3a Ask the user how old the employee is
        age = Age();

        switch(choice) {
            case 1:
                hEmp = malloc(sizeof(struct HourlyEmployee*)); 
                ePtr = malloc(sizeof(struct HourlyEmployee*)); 
                printf("What is your pay rate ? "); // #3b
                scanf ("%lf",&payRate);
                printf("What is your hours? ");
                scanf ("%lf",&hours);
                Construct_Hourly(hEmp); // #3d
                hEmp->age = age;
                hEmp->hourly_rate = payRate;
                hEmp->hours = hours;
                ePtr = (struct Employee*) hEmp; // #3e
                ((void(*)(struct Employee* ))Vtable_Hourly[0])((struct Employee* )ePtr);
                paid = ((double(*)(struct Employee* ))Vtable_Hourly[1])((struct Employee* )ePtr);
                printf("Money I made: $%.2f\n\n", paid ); 
                break;
            case 2:
                cEmp = malloc(sizeof(struct CommissionEmployee*));
                ePtr = malloc(sizeof(struct CommissionEmployee*));
                printf("What is your amount of sales? "); // #3c
                scanf ("%lf",&amtSales);
                Construct_Commission(cEmp);
                cEmp->age = age;
                cEmp->sales_amount = amtSales;
                ePtr = (struct Employee*) cEmp; // #3e
                ((void(*)(struct Employee* ))Vtable_Commission[0])((struct Employee* )ePtr);
                paid = ((double(*)(struct Employee* ))Vtable_Commission[1])((struct Employee* )ePtr);
                printf("Money I made: $%.2f\n\n", paid );          
                break;
            case 3:
                sEmp = malloc(sizeof(struct SeniorSalesman*));
                ePtr = malloc(sizeof(struct SeniorSalesman*));
                printf("What is your amount of sales? "); // #3c
                scanf ("%lf",&amtSales);
                Construct_Senior(sEmp);
                sEmp->age = age;
                sEmp->sales_amount = amtSales;
                ePtr = (struct Employee*) sEmp; // #3e
                ((void(*)(struct Employee* ))Vtable_Senior[0])((struct Employee* )ePtr);
                paid = ((double(*)(struct Employee* ))Vtable_Senior[1])((struct Employee* )ePtr);
                printf("Money I made: $%.2f\n\n", paid );
                break;
            default:
                printf("Please re-enter an appropriate option from 1 to 3 \n");
        }

        ePtr = NULL;
        printf("Do you want to continue? (enter -1 for NO) ");
        scanf ("%d",&cont);
    }

    // #5 Free all variables 
    free(ePtr); 
    free(hEmp); 
    free(cEmp); 
    free(sEmp); 
};