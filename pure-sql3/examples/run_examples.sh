# runs all the examples as a quick check to see that things don't blow up
# after a change to sql3.pure or sql3util.c

# change -x to -i to work interactively with each example

for f in sql3*.pure
do
  pure -x $f 
done