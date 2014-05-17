#!/usr/bin/perl

# Parse the LuaDoc files inside folder "data/"

use strict;
use warnings;

use Data::Dumper;
use File::Slurp;
use File::Spec;
use Text::Trim;



sub parseFile {
  my $fileName = shift;

  my @file = read_file($fileName);

  my @documents = ();

  my @currentObjectBeingParsed = ();

  while (scalar @file > 0) {
    my $line = shift @file;

    if ($line =~ /^---/) {
      my $state = 1;

      if (scalar @currentObjectBeingParsed > 0) {
        push @documents, [ @currentObjectBeingParsed ];
      }

      @currentObjectBeingParsed = ();
    } else {
      $line =~ s/^-- ?//;     # Remove leading "--"
      rtrim $line;            # Remove leading and trailing whitespace
      push @currentObjectBeingParsed, $line;
    }
  }

  # Remove the items from "documentation" that don't contain the
  # string "@function" in them.
  #
  # This can happen when the documentation is simply describing some
  # "module" or "global", etc.
  my @returnValue = ();

  for my $item (@documents) {
    if ( grep { m/\@function/ } @{ $item } ) {
      push @returnValue, $item;
    }
  }

  return @returnValue;
}

sub findFunctionName {
  my $documentLines = shift;

  my @lines = @{ $documentLines };

  my $functionLine = (grep { m/\@function/ } @lines)[0];

  # TODO: Sometimes @lines doesn't have a "@function" string anywhere
  # Need to see what is happening in those cases
  if (not $functionLine) {
    return undef;
  }

  my $functionName;
  my $parentModule;

  if ($functionLine =~ m/\@function \[parent=#(.*?)\] (.*)$/) {
    $parentModule = $1;
    $functionName = $2;
  }

  return sprintf("%s.%s", $parentModule, $functionName);
}

opendir(my $dh, "./data");



my @files = map { File::Spec->catfile('data', $_) } # readdir doesn't return the folder their in (see perldoc -f readdir)
            grep { /\.lua$/ }
            readdir($dh);

my @documents = ();

foreach my $file (@files) {
  push @documents, parseFile($file);
}

# The lines pushed here will be written to "../lua-documentation.el" at the end
open(my $OUTPUT_FILE, '>', 'lua-documentation.el');
my @output;

print $OUTPUT_FILE ";; This is a automatically generated file created by ./lua-documentation/parseLuaDoc.pl\n";
print $OUTPUT_FILE ";; Copy and paste the contents of this file into the variable\n";
print $OUTPUT_FILE ";; `auto-complete-lua--builtin-functions' in file \"auto-complete-lua.el\"\n";
print $OUTPUT_FILE "'(\n";        # The start of the elisp list

foreach my $documentedFunction (@documents) {
  my $functionName = findFunctionName($documentedFunction);

  next unless $functionName;

  my $functionDocumentation = join("\n", @{ $documentedFunction });

  $functionDocumentation =~ s/\\/\\\\/g;   # Quote the \ char
  $functionDocumentation =~ s/\"/\\"/g;    # Quote the " char

  print $OUTPUT_FILE sprintf("(\"%s\" . \"%s\")\n", $functionName, $functionDocumentation);
}

# This ones aren't detected in the "data/*.lua" files, so we hardcoded then here
print $OUTPUT_FILE "(\"debug.setfenv\" . \"debug.setfenv (object, table)\")\n";
print $OUTPUT_FILE "(\"debug.getfenv\" . \"debug.getfenv (o)\")\n";
print $OUTPUT_FILE "(\"os.tmpname\" . \"os.tmpname ()\")\n";
print $OUTPUT_FILE "(\"math.tanh\" . \"math.tanh (x)\")\n";
print $OUTPUT_FILE "(\"math.log10\" . \"math.log10 (x)\")\n";
print $OUTPUT_FILE "(\"table.maxn\" . \"table.maxn (table)\")\n";
print $OUTPUT_FILE "(\"string.upper\" . \"string.upper (s)\")\n";
print $OUTPUT_FILE "(\"package.seeall\" . \"package.seeall (module)\")\n";
print $OUTPUT_FILE "(\"require\" . \"require (modname)\")\n";
print $OUTPUT_FILE "(\"module\" . \"module (name [, ...])\")\n";
print $OUTPUT_FILE "(\"coroutine.yield\" . \"coroutine.yield (...)\")\n";
print $OUTPUT_FILE "(\"xpcall\" . \"xpcall (f, err)\")\n";
print $OUTPUT_FILE "(\"unpack\" . \"unpack (list [, i [, j]])\")\n";
print $OUTPUT_FILE "(\"type\" . \"type (v)\")\n";
print $OUTPUT_FILE "(\"tostring\" . \"tostring (e)\")\n";
print $OUTPUT_FILE "(\"tonumber\" . \"tonumber (e [, base])\")\n";
print $OUTPUT_FILE "(\"setmetatable\" . \"setmetatable (table, metatable)\")\n";
print $OUTPUT_FILE "(\"setfenv\" . \"setfenv (f, table)\")\n";
print $OUTPUT_FILE "(\"select\" . \"select (index, ...)\")\n";
print $OUTPUT_FILE "(\"rawset\" . \"rawset (table, index, value)\")\n";
print $OUTPUT_FILE "(\"rawget\" . \"rawget (table, index)\")\n";
print $OUTPUT_FILE "(\"rawequal\" . \"rawequal (v1, v2)\")\n";
print $OUTPUT_FILE "(\"print\" . \"print (...)\")\n";
print $OUTPUT_FILE "(\"pcall\" . \"pcall (f, arg1, ...)\")\n";
print $OUTPUT_FILE "(\"pairs\" . \"pairs (t)\")\n";
print $OUTPUT_FILE "(\"next\" . \"next (table [, index])\")\n";
print $OUTPUT_FILE "(\"loadstring\" . \"loadstring (string [, chunkname])\")\n";
print $OUTPUT_FILE "(\"loadfile\" . \"loadfile ([filename])\")\n";
print $OUTPUT_FILE "(\"load\" . \"load (func [, chunkname])\")\n";
print $OUTPUT_FILE "(\"ipairs\" . \"ipairs (t)\")\n";
print $OUTPUT_FILE "(\"getmetatable\" . \"getmetatable (object)\")\n";
print $OUTPUT_FILE "(\"getfenv\" . \"getfenv ([f])\")\n";
print $OUTPUT_FILE "(\"error\" . \"error (message [, level])\")\n";
print $OUTPUT_FILE "(\"dofile\" . \"dofile ([filename])\")\n";
print $OUTPUT_FILE "(\"collectgarbage\" . \"collectgarbage ([opt [, arg]])\")\n";
print $OUTPUT_FILE "(\"assert\" . \"assert (v [, message])\")\n";



print $OUTPUT_FILE ")\n";

close($OUTPUT_FILE);
