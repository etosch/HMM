#!/usr/bin/perl
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

sub parse_data{
    # 0: infile 1: outdir, 2: state index 3: obs index
    open FILE, $_[0] or die("Could not open file");
    my @file = <FILE>;
    @sents = split(/\n\n/,join("", @file));
    $int = 0;
    foreach (@sents){
	$outfile = "/sent$int";
	$outstring = "(";
	@emission_seq = split(/\n/, $_); 
	foreach(@emission_seq){
	    @data = split(/\s+/, $_);
	    $outstring = "$outstring {:state \"@data[$_[2]]\", :obs \"@data[$_[3]]\"}";
	}
	$outstring="$outstring)";
	open(MYFILE, ">$_[1]$outfile");
	print MYFILE $outstring;
	close(MYFILE);
	$int = $int + 1;
    }
    close(FILE);
}
# if the output directories in resources does not exist, create it
# preprocessing files creates the maps of feature observations and correct states
# I can later combine the resulting maps to make feature vectors. 
@dirs = ("../resources/BIO"
	 , "../resources/BIO/POS"
	 , "../resources/BIO/lexeme"
	 , "../resources/BIO/POS/training"
	 , "../resources/BIO/POS/testing"
	 , "../resources/BIO/lexeme/training"
	 , "../resources/BIO/lexeme/testing");

foreach (@dirs){
    if (&replace_dir($_)){
	mkdir($_);
    }
}

$training = "../resources/BIO/wsj_15_18_train";
$testing = "../resources/BIO/wsj_20_test";

&parse_data($testing, "../resources/BIO/POS/testing", 0, 2);
&parse_data($training, "../resources/BIO/POS/training", 0, 2);
&parse_data($testing, "../resources/BIO/lexeme/testing", 0, 1);
&parse_data($training, "../resources/BIO/lexeme/training", 0, 1);

