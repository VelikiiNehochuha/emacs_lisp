"""
пример общего метода на python
http://wiki.c2.com/?ExpressionProblem
"""
# классы создаем как и раньше
class Package:
    def __init__(self, name, price, volume):
        self.name = name
        self.price = price
        self.volume = volume

class OrderItem:
    def __init__(self, package):
        self.package = package

class Order:
    def __init__(self, plan, items):
        self.plan = plan
        self.items = items

class CustomOrderItem:
    def __init__(self, package, count):
        self.count = count
        self.package = package

class CustomOrder:
    def __init__(self, plan, items):
        self.plan = plan
        self.items = items

pac1 = Package('test', 10.0, 5.0)
pac2 = Package('test2', 12.0, 4.0)
oi1 = OrderItem(pac1)
oi2 = OrderItem(pac2)
coi1 = CustomOrderItem(pac1, 5)
coi2 = CustomOrderItem(pac2, 3)
co = CustomOrder('start', [coi1, coi2])
o = Order('start', [oi1, oi2])

# общие метод get_price
def type_tag(x):
    return type_tag.tags[type(x)]
type_tag.tags = {Order: 'order', CustomOrder: 'customorder',
                 Package: 'package', CustomOrderItem: 'customorderitem',
                 OrderItem: 'orderitem'}

def get_price(x):
    types = type_tag(x)
    return get_price.implementations[types](x)

get_price.implementations = {}
get_price.implementations = {
    'orderitem': lambda x: get_price(x.package),
    'order': lambda x: sum(map(get_price, x.items)),
    'package': lambda x: x.price,
    'customorderitem': lambda x: x.count * get_price(x.package),
    'customorder': lambda x: sum(map(get_price, x.items))
}


total_sum = sum(map(get_price, [o, co]))
print(total_sum)
