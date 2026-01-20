/* ------------------------------------- */
/* horse racing prediction program       */
/* ------------------------------------- */
// function that will be used as methods

function calculate_horse()
{
  /* parse horse line and find form and name */
  var pattern = /^(\d{1,4})\s+([\d-PBRFU\/]*)\s+/;

  var result = this.the_str.match(pattern);

  if (result == null) {
    // pattern didn't match
    alert("Couldn't match line for "+this.the_str);
  }

  this.form_str = result[2];

  if (result[2].length == 0) {
    // no form found so set rank to some default
    this.rank = 5;
  }
  else { // calculate average form
    var curr_tot = 0;

    for (i=0; i<this.form_str.length; i++)
    {
      switch (this.form_str.charAt(i)) {
      case '0':
        curr_tot = curr_tot + 10;
        break;
      case 'P': // pulled up
        curr_tot = curr_tot + 13;
        break;
      case 'F': // fell
        curr_tot = curr_tot + 15;
        break;
      case 'U': // unseated rider
        curr_tot = curr_tot + 10;
        break;
      case '-': // end of season
        curr_tot = curr_tot + 8;
        break;
      case '/': // end of year season if horse didn't race last season
        curr_tot = curr_tot + 3;
        break;
      case 'B': // not sure
        curr_tot = curr_tot + 10;
        break;
     case 'R': // refused to run
        curr_tot = curr_tot + 5;
        break;
      default:
        // must be some kind of place
        curr_tot = curr_tot + this.form_str.charCodeAt(i) - 48;
        break;
      }
    }
    this.rank = curr_tot / this.form_str.length;
  }
}


// define a constructor
function simple_horse(sline)
{
  // regular expression to parse horse line
  this.the_pattern = /^(\d+)\s+([\d-PFU]+)\s+(\w+)\s+([\w+[\s+]\.[\s+]]+\w+)\s+(\d+)\s+(\d+)\s+(\d+-\d+)\s+([\w+[\s+]\.[\s+]]+\w+)/;

  this.the_str = sline;
  this.form_str;
  this.rank;
  this.name;

  this.calculate = calculate_horse;
}

// read in lines of horse data from text area
function read_horse_lines()
{
  var lineup = new Array();
  var blank_pattern = /^\s*$/;

  // split text data into rows
  horse_rows = document.horselist.thehorselist.value.split(/\n/);

  if (horse_rows.length < 3)
     alert("You need to copy the horse data from somewhere");
  else {
    win_rank = 99999;

    for (hnum=0; hnum<horse_rows.length; hnum++)
    {
      // check for any blank lines ignore horse calculation if found
      var blank_result = horse_rows[hnum].match(blank_pattern);

      if (blank_result == null) {

        var new_horse = new simple_horse(horse_rows[hnum]);

        lineup[hnum] = new_horse;

        // call calculate routine
        lineup[hnum].calculate();

        if (lineup[hnum].rank < win_rank) {
          win_rank = lineup[hnum].rank;
          win_horse = lineup[hnum].the_str;
        }
      }
    }

  alert("the winner will be : "+win_horse);
  }
}

function clear_horses()
{
  document.horselist.thehorselist.value = "";
}
                
