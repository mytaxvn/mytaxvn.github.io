rm -dfr bin
rm -dfr obj
rm -dfr Core/bin
rm -dfr Core/obj
rm -dfr Test/bin
rm -dfr Test/obj
rm -dfr dist

rm -dfr node_modules

dotnet fable clean --yes
rm -dfr fable_modules
