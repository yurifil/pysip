from pysip import PySIPException


class RouteSetError(PySIPException):
    pass


class RouteSet(object):
    def __init__(self):
        self.routes = list()

    def __getitem__(self, item):
        return self.routes[item]

    def __repr__(self):
        return ', '.join([str(r) for r in self.routes])

    def __eq__(self, other):
        if isinstance(other, RouteSet):
            return self.routes == other.routes
        return NotImplemented

    def is_empty(self):
        return len(self.routes) == 0

    def add_first(self, route):
        self.routes.insert(0, route)

    def add_last(self, route):
        self.routes.append(route)

    def remove_first(self):
        if not self.is_empty():
            del self.routes[0]
        else:
            raise RouteSetError(f'Cannot remove first route: empty route set')

    def remove_last(self):
        if not self.is_empty():
            del self.routes[-1]
        else:
            raise RouteSetError(f'Cannot remove last route: empty route set')

    @property
    def first(self):
        if not self.is_empty():
            return self.routes[0]
        raise RouteSetError(f'Cannot get first route: empty route set.')

    @property
    def last(self):
        if not self.is_empty():
            return self.routes[-1]
        raise RouteSetError(f'Cannot get last route: empty route set.')

    def append(self, route):
        self.add_last(route)
