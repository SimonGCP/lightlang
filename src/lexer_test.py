from lexer import Scanner

def test(actual, expected, number):
    if (expected == actual):
        print("Passed test", number)
    else:
        print("Failed test", number)
    
    return expected == actual

f1 = open("test_files/test.light").read()
s1 = Scanner(f1)
test(s1.scanTokens(), False, 1)

f2 = """
m() {
    p(hello)
}
"""
s2 = Scanner(f2)
test(s2.scanTokens(), False, 2)