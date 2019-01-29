from pysip.message.branch import is_rfc3261, Branch, BranchKey, make_rfc3261, assemble
import pytest


@pytest.mark.parametrize('branch', [b'z9hG4bK776asdhds', b'z9hG4bK'])
def test_is_rfc3261(branch):
    assert is_rfc3261(Branch(branch))


@pytest.mark.parametrize('branch', [b'z9hg4bka', b'_z9hG4bK', b'_', b'z'])
def test_not_rfc3261(branch):
    assert not is_rfc3261(Branch(branch))


@pytest.mark.parametrize('branch', [b'z9hg4bk776asdhds', b'z9hg4bk'])
def test_key_is_rfc3261(branch):
    assert is_rfc3261(BranchKey(Branch(branch)))


@pytest.mark.parametrize('branch', [b'_z9hG4bK', b'_', b'z'])
def test_key_not_rfc3621(branch):
    assert not is_rfc3261(BranchKey(Branch(branch)))


def test_eq():
    assert Branch(b'z9hG4bK776asdhds') == Branch(b'z9hg4bk776asdhds')


def test_ne():
    assert Branch(b'z9hG4bK77_6asdhds') != Branch(b'z9hG4bK776asdhds')


def test_branch_key_idempotent():
    key = BranchKey(Branch(b'z9hG4bK776asdhds'))
    assert key == BranchKey(key)


@pytest.mark.parametrize('branch', [b'aaaa', b'z9hG4bKaaaa'])
def test_make_rfc3261(branch):
    assert is_rfc3261(make_rfc3261(branch))


@pytest.mark.parametrize('branch', [b'z9hg4bk776asdhds', b'z9hg4bk', b'_z9hG4bK', b'_', b'z'])
def test_reqssemble(branch):
    assert branch == Branch(branch).binary

