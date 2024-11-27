import { display, head, list, tail } from 'sicp';

const p = list("I", "love", "sicp");
display(head(tail(p)));