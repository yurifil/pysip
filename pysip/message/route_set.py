from pysip import PySIPException


class RouteSetError(PySIPException):
    pass


class RouteSet(object):
    def __init__(self):
        self.routes = list()

    def __repr__(self):
        return ', '.join(self.routes)

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

'''

-spec foldl(Fun, any(), route_set()) -> any() when
      Fun :: fun((route(), any()) -> any()).
foldl(Fun, Init, {route_set, R}) ->
    lists:foldl(Fun, Init, R).

'''