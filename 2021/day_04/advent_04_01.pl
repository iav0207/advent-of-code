#!/usr/bin/perl

my $line = <STDIN>;
chomp $line;
my @queue = split ',', $line;
my %idx = ();
for my $i (0..scalar(@queue)-1) { $idx{$queue[$i]} = $i; }

# while (my ($k,$v)=each %idx){print "idx[$k] = $v\n"}

# minimum turn # to win
my $winner_elim = scalar @queue;
my @winner_board = ();

# min/max of two values
sub max ($$) { $_[$_[0] < $_[1]] }
sub min ($$) { $_[$_[0] > $_[1]] }

sub process_board {
  <>;
  my @board;
  my @elim = (); # 0..4 - row elimination, 5..9 - col elimination
  my $board_elim = scalar @queue;
  for ((0..9)) { push @elim, 0; }
  for my $i ((0..4)) {
    $line = <>;
    my @row = split ' ', $line;
    push @board, \@row;
    for my $j ((0..4)) {
      my $cell_val = $row[$j];
      my $cell_elim = $idx{$cell_val};
      $elim[$i] = max $elim[$i], $cell_elim;
      $elim[$j+5] = max $elim[$j+5], $cell_elim;
    }
  }

  for (@elim) { $board_elim = min $board_elim, $_; }

  if ($board_elim < $winner_elim) {
    $winner_elim = $board_elim;
    @winner_board = @board;
  }
}

until (eof()) { process_board(); }

# hash the marked numbers
my %marked = map { $_ => 1 } @queue[0..$winner_elim];
my $last_marked = $queue[$winner_elim];

print "This board wins at turn $winner_elim:\n";
my $unmarked_sum = 0;
for my $i ((0..4)) {
  print "  ";
  for my $j ((0..4)) {
    my $it = $winner_board[$i][$j];
    if (exists($marked{$it})) {
      printf '[%2d] ', $it;
    } else {
      printf ' %2d  ', $it;
      $unmarked_sum += $it;
    }
  }
  print "\n";
}

print "Sum of all unmarked numbers: $unmarked_sum\n";
print "Last called number: $last_marked\n";
my $score = $unmarked_sum * $last_marked;
print "Score: $score\n";

