#lang scheme

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;CUSTOMER LIST 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define customer_list '((John_Smith 35 New_York)
                        (Alice_Johnson 28 Los_Angeles)
                        (Michael_Brown 45 Miami)
                        (Emily_Davis 32 Houston)
                        (Robert_Wilson 40 Miami)
                        (Sophia_Martinez 30 New_York)
                        (William_Taylor 38 Houston)
                        (Emma_White 25 Los_Angeles)
                        (James_Harris 32 Houston)
                        (Olivia_Clark 29 Los_Angeles)
                         ))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;ITEM LIST
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(define item_list '((Apples Fruits 2.30)
                         (Coffee Beverages 3.50)
                         (Bread Bakery 2.00)
                         (Milk Dairy 3.50)
                         (Bananas Fruits 1.75)
                         (Eggs Dairy 4.75)
                         (Orange_Juice Beverages 3.25)
                         (Tea Beverages 2.75)
                         (Fish Seafood 12.50)
                         (Broccoli Vegetables 1.80)
                         (Orange Fruits 1.25)
                         (Chicken Meat 7.00)
                         (Lettuce Vegetables 1.20)
                         (Pasta Pantry 3.75)
                         (Salmon Seafood 9.50)
                         (Yogurt Dairy 2.75)
                         (Bacon Meat 6.25)
                         (Cheese Dairy 5.50)
                         (Beef Meat 8.00)
                         (Potatoes Vegetables 2.50)
                         (Chicken_Soup Canned_Goods 3.50)
                         (Rice Grains 2.25)
                         (Carrots Vegetables 1.10)
                         (Spinach Vegetables 1.60)
                         (Tomatoes Vegetables 1.50)
                         (Apple_Juice Beverages 3.40)
                         (Onions Vegetables 1.20)
                         ))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;PURCHASE_LIST
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(define purchase_list '((John_Smith (Apples Coffee Bread) 15.03.2024)
                        (John_Smith (Milk Bananas) 22.03.2024)
                        (John_Smith (Eggs Orange_Juice) 29.03.2024)
                        (John_Smith (Tea Fish Broccoli Orange) 5.04.2024)
                        (John_Smith (Chicken Lettuce Pasta Salmon) 12.04.2024)
                        (Alice_Johnson (Milk Bananas) 20.03.2024)
                        (Michael_Brown (Orange_Juice Yogurt) 24.03.2024)
                        (Michael_Brown (Bacon) 28.03.2024)
                        (Michael_Brown (Coffee Bread Apples) 2.04.2024)
                        (Michael_Brown (Milk Bananas Eggs) 5.04.2024)
                        (Michael_Brown (Cheese Beef Potatoes Chicken_Soup) 10.04.2024)
                        (Emily_Davis (Chicken Lettuce) 24.03.2024)
                        (Emily_Davis (Pasta Salmon Rice Potatoes) 28.03.2024)
                        (Emily_Davis (Carrots Spinach) 1.04.2024)
                        (Robert_Wilson (Salmon Rice) 21.03.2024)
                        (Robert_Wilson (Potatoes Chicken Lettuce Pasta) 25.03.2024)
                        (Robert_Wilson (Milk Bananas Eggs Orange_Juice) 29.03.2024)
                        (Robert_Wilson (Bacon) 2.04.2024)
                        (Robert_Wilson (Fish Broccoli) 6.04.2024)
                        (Sophia_Martinez (Carrots Spinach) 26.03.2024)
                        (Sophia_Martinez (Tea Fish Broccoli Orange) 30.03.2024)
                        (William_Taylor (Beef Potatoes) 19.03.2024)
                        (William_Taylor (Chicken_Soup Tomatoes Apple_Juice Bread) 23.03.2024)
                        (Emma_White (Tomatoes Chicken_Soup) 23.03.2024)
                        (Emma_White (Milk Salmon Rice Potatoes) 27.03.2024)
                        (Emma_White (Chicken Lettuce Pasta Salmon) 31.03.2024)
                        (James_Harris (Onions Apple_Juice) 25.03.2024)
                        (James_Harris (Cheese Beef Potatoes Chicken_Soup) 29.03.2024)
                        (Olivia_Clark (Fish Broccoli) 25.03.2024)
                        (Olivia_Clark (Orange Chicken Lettuce Pasta) 29.03.2024)                  
                        ))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;Retrieve all items: Return a list of all unique items. 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(define (get-unique-items item_list)
  (if (null? item_list)
      '()
      (cons
        (car (car item_list))
        (get-unique-items (cdr item_list)))))

(write 'Unique_Items:)(get-unique-items item_list)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;Retrieve all categories: Return a list of all unique categories. 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(define (get-unique-categories item_list)
  (define (helper items seen-categories)
    (if (null? items)
        seen-categories
        (let ((category (cadr (car items))))
          (if (not (member category seen-categories))
              (let ((last-element (get-last-element seen-categories)))
                (if (or (null? last-element) (not (equal? last-element category)))
                    (helper (cdr items) (append seen-categories (list category)))
                    (helper (cdr items) seen-categories)))
              (helper (cdr items) seen-categories)))))
  (helper item_list '()))

(define (get-last-element list)
  (if (null? list)
      #f 
      (if (null? (cdr list))
          (car list)
          (get-last-element (cdr list)))))

(write 'Unique_Categories:)(get-unique-categories item_list)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;Retrieve items by category: Take a category name as input and return a list of items belonging 
;to that category. 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (find-products-by-category category item_list)
  (define (find-products-in-category items)
    (cond ((null? items) '())
          (else
           (let ((item (car items)))
             (if (equal? (cadr item) category)
                 (cons item (find-products-in-category (cdr items)))
                 (find-products-in-category (cdr items)))))))

  (find-products-in-category item_list))

;; Example usage:
(write 'Category_Name:)(find-products-by-category 'Dairy item_list)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;Get Customer Informa on: Retrieve informa on about a specific customer by providing their 
;name as an argument. 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (get-customer-info customer_name)
  (define (find-customer customers)
    (cond ((null? customers) #f)
          (else
           (let ((customer (car customers)))

             (if (equal? (car customer) customer_name)
                 customer
                 (find-customer (cdr customers)))))))

  (find-customer customer_list))

;; Example usage:
(write 'Customer_info:)(get-customer-info 'Alice_Johnson)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;Get Item Informa on: Retrieve informa on about a specific item by providing its name as an
;argument. 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (get-item-info item_name)

  (define (find-item items)
    (cond ((null? items) #f)
          (else
           (let ((item (car items)))

             (if (equal? (car item) item_name)
                 item
                 (find-item (cdr items)))))))
  (find-item item_list))

;; Example usage:
(write 'Item_info:)(get-item-info 'Milk)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;Most expensive item: Find the most expensive item.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (find-most-expensive-item item-list)
  (define (compare-items item1 item2)
    (if (> (caddr item1) (caddr item2))
        item1
        item2))
  (define (find-expensive-item-helper items current-max)
    (if (null? items)
        current-max
        (find-expensive-item-helper (cdr items)
                                    (compare-items (car items) current-max))))
  (find-expensive-item-helper (cdr item-list) (car item-list)))

(write 'Most_expensive_Item:)(find-most-expensive-item item_list)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;Cheapest item: Find the cheapest item. 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (find-cheapest-item item-list)
  (define (compare-items item1 item2)
    (if (< (caddr item1) (caddr item2))
        item1
        item2))
  (define (find-cheap-item-helper items current-min)
    (if (null? items)
        current-min
        (find-cheap-item-helper (cdr items)
                                (compare-items (car items) current-min))))
  (find-cheap-item-helper (cdr item-list) (car item-list)))

(write 'Cheapest_Item:)(find-cheapest-item item_list)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;Total cost of items: Calculate the total cost of all items purchased for a specific customer by 
;providing their name as an argument.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (find-item-price item item-list)
  (if (null? item-list)
      0
      (let ((current-item (car item-list)))
        (if (equal? item (car current-item))
            (caddr current-item)
            (find-item-price item (cdr item-list))))))

(define (calc-transaction-cost items item-list)
  (if (null? items)
      0
      (+ (find-item-price (car items) item-list)
         (calc-transaction-cost (cdr items) item-list))))


(define (total-cost-for-customer customer-name purchase-list item-list)
  (if (null? purchase-list)
      0
      (let ((current-purchase (car purchase-list)))
        (if (equal? customer-name (car current-purchase))
            (+ (calc-transaction-cost (cadr current-purchase) item-list)
               (total-cost-for-customer customer-name (cdr purchase-list) item-list))
            (total-cost-for-customer customer-name (cdr purchase-list) item-list)))))
;; Example usage:
(write 'Total_cost_for_customer:)(total-cost-for-customer 'John_Smith purchase_list item_list)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;Items bought by a specific customer: Retrieve all items bought by a specific customer. 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (items-bought-by-customer customer-name purchase-list)
  (if (null? purchase-list)
      '()       
      (let ((current-purchase (car purchase-list)))
        (if (equal? customer-name (car current-purchase))
            (append (cadr current-purchase)
                    (items-bought-by-customer customer-name (cdr purchase-list)))
            (items-bought-by-customer customer-name (cdr purchase-list))))))
;; Example usage:
(write 'Items_bought_by_customer:)(items-bought-by-customer 'Alice_Johnson purchase_list)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;Total cost of transactions: Calculate the total cost of all transactions.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (total-cost-of-transactions purchase-list item-list)
  (if (null? purchase-list)
      0
      (let ((current-purchase (car purchase-list)))
        (+ (calc-transaction-cost (cadr current-purchase) item-list)
           (total-cost-of-transactions (cdr purchase-list) item-list)))))


(write 'Total_cost_of_transactions:)(total-cost-of-transactions purchase_list item_list)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;Items purchased on a specific date: Retrieve all items purchased on a specific date. 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(define (items-purchased-on-date date purchase-list)
  (define (helper purchases)
    (if (null? purchases)
        '()
        (let ((current-purchase (car purchases)))
          (if (equal? (caddr current-purchase) date)
              (append (cadr current-purchase) (helper (cdr purchases)))
              (helper (cdr purchases))))))
  (helper purchase-list))

;; Example usage:
(write 'Item_purchased_on_date:)(items-purchased-on-date '15.03.2024 purchase_list)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;Revenue by category: Calculate the total revenue generated by each category. 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (calculate-total-revenue-by-category category purchase-list item-list)
  (define (find-item-price-in-category item-name category item-list)
    (define (helper items)
      (if (null? items)
          0  ;
          (let ((current-item (car items)))
            (if (and (equal? (car current-item) item-name)
                     (equal? (cadr current-item) category))
                (caddr current-item)
                (helper (cdr items))))))
    (helper item-list))

  (define (calc-transaction-cost items)
    (define (helper items total-cost)
      (if (null? items)
          total-cost
          (let ((item (car items)))
            (let ((price (find-item-price-in-category item category item-list)))
              (helper (cdr items) (+ total-cost price))))))
    (helper items 0))

  (define (helper purchases total-revenue)
    (if (null? purchases)
        total-revenue
        (let ((current-purchase (car purchases)))
          (helper (cdr purchases)
                  (+ total-revenue
                     (calc-transaction-cost (cadr current-purchase)))))))
  

  (helper purchase-list 0))

; Example usage:
(write 'Fruits:)(calculate-total-revenue-by-category 'Fruits purchase_list item_list)
(write 'Beverages:)(calculate-total-revenue-by-category 'Beverages purchase_list item_list)
(write 'Bakery:)(calculate-total-revenue-by-category 'Bakery purchase_list item_list)
(write 'Dairy:)(calculate-total-revenue-by-category 'Dairy purchase_list item_list)
(write 'Seafood:)(calculate-total-revenue-by-category 'Seafood purchase_list item_list)
(write 'Vegetables:)(calculate-total-revenue-by-category 'Vegetables purchase_list item_list)
(write 'Meat:)(calculate-total-revenue-by-category 'Meat purchase_list item_list)
(write 'Pantry:)(calculate-total-revenue-by-category 'Pantry purchase_list item_list)
(write 'Canned_Goods:)(calculate-total-revenue-by-category 'Canned_Goods purchase_list item_list)
(write 'Grains:)(calculate-total-revenue-by-category 'Grains purchase_list item_list)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;Most popular category: Determine the most popular category based on the number of items 
;sold. 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(define (find-category item)
  (define (find-helper lst)
    (if (null? lst)
        '()
        (let ((current-item (car lst)))
          (if (equal? (car current-item) item)
              (cdr current-item)
              (find-helper (cdr lst))))))
  (find-helper item_list))

(define (count-items purchases)
  (define (count-helper purchases categories)
    (if (null? purchases)
        categories
        (let ((items (cadr (car purchases))))
          (define (accumulate-categories items categories)
            (if (null? items)
                categories
                (let ((category (find-category (car items))))
                  (accumulate-categories (cdr items) (cons category categories)))))
          (count-helper (cdr purchases) (accumulate-categories items categories)))))
  (count-helper purchases '()))

(define (count-categories categories)
  (define (count-helper categories counts)
    (if (null? categories)
        counts
        (let ((category (car categories)))
          (define (update-counts category counts)
            (if (null? counts)
                (list (cons category 1))
                (let ((current (car counts)))
                  (if (equal? (car current) category)
                      (cons (cons category (+ (cdr current) 1)) (cdr counts))
                      (cons current (update-counts category (cdr counts)))))))
          (count-helper (cdr categories) (update-counts category counts)))))
  (count-helper categories '()))

(define (find-most-popular-category counts)
  (define (find-max-category counts max-category max-count)
    (if (null? counts)
        max-category
        (let* ((current-category (caar counts))
               (current-count (cdar counts)))
          (if (> current-count max-count)
              (find-max-category (cdr counts) current-category current-count)
              (find-max-category (cdr counts) max-category max-count)))))
  (find-max-category counts '() 0))

(define (most-popular-category)
  (let* ((categories (count-items purchase_list))
         (category-counts (count-categories categories)))
    (find-most-popular-category category-counts)))

(write 'Most_Popular_Category: )(car(most-popular-category))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Items purchased by age group: Retrieve all items purchased by customers within a certain age 
; range. 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (items-purchased-by-age-range age-min age-max purchase-list)
  (define (helper purchases)
    (if (null? purchases)
        '()
        (let ((current-purchase (car purchases)))
          (let ((customer-name (car current-purchase)))
            (let ((customer-info (get-customer-info customer-name)))
              (let ((age (cadr customer-info)))
                (if (and (>= age age-min) (<= age age-max))
                    (append (cadr current-purchase) (helper (cdr purchases)))
                    (helper (cdr purchases)))))))))
  (helper purchase_list))

;; Example usage:
(write 'Age_Range:)(items-purchased-by-age-range 25 30 purchase_list)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;Average spending per transac on by loca on: Calculate the average spending per transac on 
;in each location.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(define (average-spending-per-transaction-by-location location purchase-list item-list)
  (define (helper purchases total-cost transaction-count)
    (if (null? purchases)
        (/ total-cost transaction-count)
        (let* ((current-purchase (car purchases))
               (customer-name (car current-purchase))
               (customer-info (get-customer-info customer-name)))
          (if (equal? location (caddr customer-info))
              (helper (cdr purchases)
                      (+ total-cost (calc-transaction-cost (cadr current-purchase) item-list))
                      (+ transaction-count 1))
              (helper (cdr purchases) total-cost transaction-count)))))
  (helper purchase-list 0 0))

;; Example usage:
(write 'New_York:)(average-spending-per-transaction-by-location 'New_York purchase_list item_list)
(write 'Los_Angeles:)(average-spending-per-transaction-by-location 'Los_Angeles purchase_list item_list)
(write 'Miami:)(average-spending-per-transaction-by-location 'Miami purchase_list item_list)
(write 'Houston:)(average-spending-per-transaction-by-location 'Houston purchase_list item_list)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;