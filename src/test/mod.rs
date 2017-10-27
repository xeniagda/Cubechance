extern crate rand;
extern crate statrs;

use std::time::Instant;
use std::collections::HashMap;

use wca::*;
use wca::wca_export::*;
use self::rand::Rng;
use self::statrs::distribution::Distribution;

const TEST_RESULT_LINE: &'static str = "LyonOpen2007\t333\t1\t15\t1968\t2128\tEtienne Amany\t2007AMAN01\tCote d_Ivoire\ta\t-1\t2203\t2138\t2139\t2108\tAfR\tAfR";
const TEST_COMP_LINE: &'static str = "ArenaCurucaOpen2018\tArena Curuçá Open 2018\tSão Paulo - SP\tBrazil\tA inscrição é **gratuita** e aberta a qualquer pessoa de qualquer nacionalidade. As inscrições para todas as modalidades poderão ser feitas até o dia 21 de janeiro de 2018. No dia do campeonato, as inscrições estarão abertas somente para o 3x3. Mais informações na aba \"Inscrições\".\t2018\t1\t27\t1\t27\t222 333 pyram\t[{Ronan Felipe Jorge}{mailto:ronan.jorge@hotmail.com}]\t[{Mauricio Paulino Marques Fernandes}{mailto:mauriciopmf@yahoo.com.br}]\t[Arena Curuçá](http://www.curucafutsal.com.br)\tRua Grapira, 70 - Vila Curuçá, São Miguel Paulista\t\thttps://sites.google.com/prod/view/arenaopen\tArena Curuçá Open 2018\t-23496048\t-46422484";

lazy_static! {
    static ref STATE: WcaResults = {
        let people = generate_people(200, 1000, &[&"333", "444"]);
        let mut state = WcaResults::default();
        state.people = people;

        // insert_comp(TEST_COMP_LINE.to_string(), &mut state).unwrap();

        println!("Created wca");
        state
    };
}

#[test]
fn test_place() {
    let people: Vec<_> = STATE.people.keys()
            .filter_map(|id| STATE.ext_person(id))
            .collect();

    assert!(people.len() > 0, "There are people");
    
    let start = Instant::now();
    for person in people.iter() {

        let pp = person.place_prob(&people, "333");
        assert_eq!(pp.len(), people.len(), "Correct amount of places");
        let sum = pp.iter().sum::<f64>();
        assert!(f64::abs(sum - 1f64) < 0.0001, format!("Sum {} == 1", sum));

    }
    let dur = start.elapsed();
    
    println!("Took {:?}", dur);
    assert!(dur.as_secs() < 3, format!("Time {:?} < 3", dur));
}

fn generate_people(amount: u16, event_amount: u16, events: &[&str]) -> HashMap<String, WcaPerson> {
    let mut rng = rand::thread_rng();

    let mut res = HashMap::new();
    for i in 0..amount {
        let mut results = HashMap::new();
        for event in events.iter() {
            let mean = rng.gen_range(0f64, 3000f64);
            let dev = rng.gen_range(50f64, 400f64);
            
            let dist = statrs::distribution::Normal::new(mean, dev).unwrap();
            let mut times = Vec::with_capacity(event_amount as usize);
            (0..event_amount)
                .for_each(|_x|
                         times.push(Time::Time(f64::abs(dist.sample(&mut rng)) as u16))
                         );
            results.insert(event.to_string(), times);
        }
        let person = WcaPerson { name: i.to_string(), times: results };
        res.insert(format!("2015XXXX{:02}", i), person);
    }
    res
}
