package core

import javax.swing._
import java.awt._
import javax.swing.event._
import java.awt.event._
import java.text._
import java.util.Random

object Main extends App with ChangeListener with ActionListener with KeyListener {
  
	def toWords (n: Int): String = {
	   n match {
	    case 0 => ""
	    case 1 => " one"
	    case 2 => " two"
	    case 3 => " three"
	    case 4 => " four"
	    case 5 => " five"
	    case 6 => " six"
	    case 7 => " seven"
	    case 8 => " eight"
	    case 9 => " nine"
	    case 10 => " ten"
	    case 11 => " eleven"
	    case 12 => " twelve"
	    case 13 => " thirteen"
	    case 15 => " fifteen"
	    case x if x > 13 && x < 20 => toWords(n - 10) + "teen"
	    
	    case x if x >= 1000000000 => toWords(n / 1000000000) + " billion" + toWords(n - n / 1000000000 * 1000000000)
	    case x if x >= 1000000 => toWords(n / 1000000) + " million" + toWords(n - n / 1000000 * 1000000)
	    case x if x >= 1000 => toWords(n / 1000) + " thousand" + toWords(n - n / 1000 * 1000)
	    case x if x >= 100 => toWords(n / 100) + " hundred" + toWords(n - n / 100 * 100)
	    case x if x >= 90 => " ninety" + toWords(n - 90)
	    case x if x >= 80 => " eighty" + toWords(n - 80)
	    case x if x >= 70 => " seventy" + toWords(n - 70)
	    case x if x >= 60 => " sixty" + toWords(n - 60)
	    case x if x >= 50 => " fifty" + toWords(n - 50)
	    case x if x >= 40 => " forty" + toWords(n - 40)
	    case x if x >= 30 => " thirty" + toWords(n - 30)
	    case x if x >= 20 => " twenty" + toWords(n - 20)
	    case _ => "out of bounds"
	  }
	}
	
	def toWords (n: Long): String = {
	  n match {
	    case x if x >= 1000000000000000000L => toWords(n / 1000000000000000000L) + " quintillion" + toWords(n - n / 1000000000000000000L * 1000000000000000000L)
	    case x if x >= 1000000000000000L => toWords(n / 1000000000000000L) + " quadrillion" + toWords(n - n / 1000000000000000L * 1000000000000000L)
	    case x if x >= 1000000000000L => toWords(n / 1000000000000L) + " trillion" + toWords(n - n / 1000000000000L * 1000000000000L)
	    case x if x >= 2147483648L => toWords(n / 1000000000L) + " billion" + toWords(n - n / 1000000000L * 1000000000L)
	    case _ => toWords(n.toInt)
	  }
	}
	
	System.out.println(toWords(Long.MaxValue))
	
	val frame = new JFrame("numgen fungen")
	val panel = new JPanel
	val slider = new JSlider(0, Int.MaxValue, Int.MaxValue / 2)
	val intbox = new JFormattedTextField(NumberFormat.INTEGER_FIELD)
	val wordlabel = new JLabel(toWords(Int.MaxValue / 2))
	val button = new JButton("Start Randomize")
	var rand = new Random
	var btoggle = false
	
	slider addChangeListener this
	intbox setValue Int.MaxValue / 2
	intbox addActionListener this
	intbox addKeyListener this
	//intbox.
	button addActionListener this
	panel setLayout new GridLayout(4,1)
	
	panel add intbox
	panel add wordlabel
	panel add slider
	panel add button
	frame add panel
	frame pack()
	frame setSize (700,200)
	frame setLocationRelativeTo null
	frame setDefaultCloseOperation JFrame.EXIT_ON_CLOSE 
	frame setVisible true

	val bThread = new Thread (new Runnable {
	  def run {
	    while(true) {
	      if(btoggle){
	    	var i = rand.nextInt(Int.MaxValue)
	    	intbox setValue i
	    	slider setValue i
	    	wordlabel setText toWords(i)
	    	Thread.sleep(200)
	      }
	      System.out.print("")
	      
	    }
	  }
	})
	bThread start
	
	def intboxChange() {
		if(intbox.getDocument().getLength() == 0 || intbox.getText.charAt(0) == '-') intbox setValue 0 
		if(intbox.getText.length > 10 || intbox.getText().toLong > Int.MaxValue.toLong ) intbox setValue Int.MaxValue 
		slider setValue intbox.getText.toInt
		if (slider.getValue == 0) wordlabel setText "zero"
		else wordlabel setText toWords(slider.getValue)
		intbox.setCaretPosition(intbox.getDocument().getLength())
	}
	
	def keyPressed(e: KeyEvent) {
	  println(e.getKeyChar() + " pressed")
	}
	def keyReleased(e: KeyEvent) {
	  println(e.getKeyChar() + " released")
	  intboxChange
	}
	def keyTyped(e: KeyEvent) {
	  println(e.getKeyChar() + " typed")
	  e getKeyChar match {
	    case '\b' | '0' | '1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9' =>
	    case _ => e consume
	  }
	}
	
	def stateChanged(e: ChangeEvent) {
	    intbox setValue slider.getValue 
	    if (slider.getValue == 0) wordlabel setText "zero" 
	    else wordlabel setText toWords(slider.getValue) 
	}
	
	def actionPerformed(e: ActionEvent){
	  if(e.getSource() == button) {
	    btoggle = !btoggle
	    if(btoggle) button setText("Stop Randomize")
	    else button setText("Start Randomize")
	  } else {
		  intboxChange
	
	  }
	}
	
}