import java.awt.Color.*;
import java.util.Random;

import tester.*;
import javalib.worldimages.*;
import javalib.funworld.*;


class Apple {
    Posn ctr;
    int radius;
    
    Apple(Posn ctr, int radius) {
        this.ctr = ctr;
        this.radius = radius;
    }
    
    public Apple moveDown() {
        return new Apple(new Posn (this.ctr.x, this.ctr.y + 5), this.radius);
    }
    
    public boolean onTheGround() {
       return (this.ctr.y + this.radius >= 400);
    }
    
    public Apple fall() {
        if (this.onTheGround()) {
            return new Apple (new Posn (new Random().nextInt(381) + 10, 390), this.radius);
        }
        else {
            return this.moveDown();
        }
    }
}

class Basket {
    Posn ctr;
    int radius;
    
    Basket(Posn ctr, int radius) {
        this.ctr = ctr;
        this.radius = radius;
    }
    

    public Basket moveOnKey(String ke) {
        if (ke.equals("left")) {
            return new Basket(new Posn (this.ctr.x - 5, this.ctr.y), this.radius);
        }
        else if (ke.equals("right")) {
            return new Basket(new Posn (this.ctr.x + 5, this.ctr.y), this.radius);
        }
        else 
            return this;            
    }
}

class AppleGame extends World {
    int caught;
    Apple apple;
    Basket basket;
    
    AppleGame(int caught, Apple apple, Basket basket) {
        this.caught = caught;
        this.apple = apple;
        this.basket = basket;
    }
    
    WorldImage redapple = 
            new FromFileImage(this.apple.ctr, "red-apple.png");
    
    WorldImage bg = 
            new FromFileImage(new Posn(0,0), "apple-tree.png");
    public WorldImage makeImage(){
        return new OverlayImage(new OverlayImage )
    }
 
    /*
    WorldImage yellowapple =
            new FromFileImage(this.apple.ctr, "yellow-apple");
    
    WorldImage smallred = 
            new FromFileImage(this.apple.ctr, "")
    */
    
    public boolean caughtApple() {
        return (this.basket.ctr.x >= this.apple.ctr.x - this.apple.radius) &&
                (this.basket.ctr.x <= this.apple.ctr.x + this.apple.radius) &&
                (this.basket.ctr.y >= this.apple.ctr.y - this.apple.radius) &&
                (this.basket.ctr.y <= this.apple.ctr.y + this.apple.radius);
    }
    
    public AppleGame onTick() {
        if (this.caughtApple()) {
            return new AppleGame ((this.caught + 1), 
                    (new Apple (new Posn (new Random().nextInt(381) + 10, 390), this.apple.radius)),
                    this.basket);
        }
        else {
            return new AppleGame (this.caught, this.apple.fall(), this.basket);
        }
    }
    
    public AppleGame onKeyEvent(String ke) {
        return new AppleGame (this.caught, this.apple, this.basket.moveOnKey(ke));
    }
    
    public WorldEnd worldEnds() {
        if ()
    }
    
}

