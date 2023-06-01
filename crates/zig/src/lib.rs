mod world_generator;
mod interface_generator;
mod function_generator;

use wit_component::StringEncoding;
use world_generator::WorldGenerator;

#[derive(Default, Debug, Clone)]
#[cfg_attr(feature = "clap", derive(clap::Args))]
pub struct Opts {
    /// Set component string encoding
    #[cfg_attr(feature = "clap", arg(long, default_value_t = StringEncoding::default()))]
    pub string_encoding: StringEncoding,
}

impl Opts {
    pub fn build(&self) -> Box<dyn wit_bindgen_core::WorldGenerator> {
        let mut world_gen = WorldGenerator::default();
        world_gen.opts = self.clone();
        Box::new(world_gen)
    }
}
