#!usr/bin/perl
use File::Path;

sub replace_dir{
    if (-d $_[0]){
	print "Replace existing $_[0] (y/n):";
	$ans = <>;
	if ($ans eq "y"){
	    rmtree($_);
	    return 1;
	}else{
	    return 0;
	}
    }else{
	return 1;
    }
}

@output_dirs = ("../resources/treebank"
	 , "../resources/treebank/lexeme"
	 , "../resources/treebank/lexeme/all"
	 , "../resources/treebank/lexeme/training"
	 , "../resources/treebank/lexeme/testing");

foreach(@output_dirs){
    if(&replace_dir($_)){
	mkdir($_);
    }
}

$treebank = "../resources/treebank-source/treebank-2/tagged/wsj/";
@input_files = `find $treebank | grep .pos`;
chomp(@input_files);
$int = 0;
foreach(@input_files){
    open FILE, $_ or die("Could not open file");
    my @file = <FILE>;
    @sents = grep(/.*\w+.*/, split(/\n\n/, join("", @file)));
    foreach (@sents){
	$outfile = "../resources/treebank/lexeme/all/sent$int";
	$outstring = "(";
	@sent = split(/\s+/, $_);
	foreach(@sent){
	    @emission = grep(/\//, split(/w+?\/w+?/, $_));
	    if(@emission){
		@e = split(/\//, $_);
		$state = @e[1];
		$state = s/\\/""/g;
		$state = s/\":/"\":\""/g;
		$obs = @e[0];
		$obs = s/\\/""/g;
		$obs = s/\":/"\":\""/g;
		$outstring="$outstring {:state \"$state\", :obs \"$obs\"}";
	    }
	}
	$outstring="$outstring)";
	open(MYFILE, ">$outfile");
	print MYFILE $outstring;
	close(MYFILE);
	$int=$int+1;
    }
    close(FILE);
}
