export class DataSource{
    constructor(){
        let i = 0;
        this._id = setInterval(()=>this.emit(i++),200);
    }

    private _id;
    public next;
    public complete;
    public error;

    emit(n){
        const limit = 10;
        if(this.next){
            this.next(n);
        }

        if(n == (limit-3)){
            if(this.error){
                this.error("fck");
            }
        }

        if(n == limit){
            if(this.complete){
                this.complete();
            }
            this.destory();
        }
    }

    destory(){
        clearInterval(this._id);
    }
}
