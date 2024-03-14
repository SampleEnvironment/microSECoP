// -----------------------------------------------------------------------------
// Rust SECoP playground
//
// This program is free software; you can redistribute it and/or modify it under
// the terms of the GNU General Public License as published by the Free Software
// Foundation; either version 2 of the License, or (at your option) any later
// version.
//
// This program is distributed in the hope that it will be useful, but WITHOUT
// ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
// FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
// details.
//
// You should have received a copy of the GNU General Public License along with
// this program; if not, write to the Free Software Foundation, Inc.,
// 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
//
// Module authors:
//   Georg Brandl <g.brandl@fz-juelich.de>
//
// -----------------------------------------------------------------------------

use syn::*;
use quote::{ToTokens, quote, format_ident};
use proc_macro2::TokenStream;
use darling::{FromMeta, FromDeriveInput};


#[derive(FromDeriveInput, Debug)]
#[darling(attributes(secop), supports(struct_named))]
struct Module {
    #[darling(multiple)]
    interface: Vec<String>,
    #[darling(multiple)]
    feature: Vec<String>,
    #[darling(multiple)]
    param: Vec<Param>,
    #[darling(multiple)]
    command: Vec<Command>,
}

#[derive(FromMeta, Debug)]
struct Param {
    name: String,
    doc: String,
    datainfo: DataInfo,
    readonly: bool,
    group: Option<String>,
    // internal
    polling: Option<bool>,
    generate_accessors: Option<bool>,
}

#[derive(FromMeta, Debug)]
struct Command {
    name: String,
    doc: String,
    argument: DataInfo,
    result: DataInfo,
    group: Option<String>,
}

#[derive(FromMeta, Debug)]
enum DataInfo {
    Null {},
    Bool {},
    Double { min: Option<f64>, max: Option<f64>,
             unit: Option<String>, fmtstr: Option<String>,
             abs_res: Option<f64>, rel_res: Option<f64> },
    Scaled { scale: f64, min: i64, max: i64,
             unit: Option<String>, fmtstr: Option<String>,
             abs_res: Option<f64>, rel_res: Option<f64> },
    Int { min: i64, max: i64 },
    Str { minchars: Option<usize>, maxchars: usize, is_utf8: Option<bool> },
    Blob { minbytes: Option<usize>, maxbytes: usize },
    Enum_ { #[darling(multiple)] member: Vec<EnumMember> },
    Array { minlen: usize, maxlen: usize, members: Box<DataInfo> },
    Tuple { #[darling(multiple)] member: Vec<DataInfo> },
    Struct { #[darling(multiple)] member: Vec<StructMember> },
    Rust(String),
}

fn optionize<T: ToTokens>(val: &Option<T>) -> TokenStream {
    match *val {
        None => quote!(None),
        Some(ref v) => quote!(Some(#v)),
    }
}

impl ToTokens for DataInfo {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        match self {
            DataInfo::Null {} => quote!(usecop::DataInfo::Null),
            DataInfo::Bool {} => quote!(usecop::DataInfo::Bool),
            DataInfo::Double { min, max, unit, fmtstr, abs_res, rel_res } => {
                let min = optionize(min);
                let max = optionize(max);
                let unit = optionize(unit);
                let fmtstr = optionize(fmtstr);
                let abs_res = optionize(abs_res);
                let rel_res = optionize(rel_res);
                quote!(usecop::DataInfo::Double {
                    min: #min, max: #max, unit: #unit, fmtstr: #fmtstr,
                    abs_res: #abs_res, rel_res: #rel_res
                })
            }
            DataInfo::Scaled { scale, min, max, unit, fmtstr, abs_res, rel_res } => {
                let unit = optionize(unit);
                let fmtstr = optionize(fmtstr);
                let abs_res = optionize(abs_res);
                let rel_res = optionize(rel_res);
                quote!(usecop::DataInfo::Scaled {
                    scale: #scale, min: #min, max: #max, unit: #unit, fmtstr: #fmtstr,
                    abs_res: #abs_res, rel_res: #rel_res
                })
            }
            DataInfo::Int { min, max } => quote!(usecop::DataInfo::Int {
                min: #min, max: #max
            }),
            DataInfo::Str { minchars, maxchars, is_utf8 } => {
                let minchars = optionize(minchars);
                let is_utf8 = is_utf8 == &Some(true);
                quote!(usecop::DataInfo::Str {
                    minchars: #minchars, maxchars: #maxchars, is_utf8: #is_utf8
                })
            }
            DataInfo::Blob { minbytes, maxbytes } => {
                let minbytes = optionize(minbytes);
                quote!(usecop::DataInfo::Blob {
                    minbytes: #minbytes, maxbytes: #maxbytes
                })
            }
            DataInfo::Enum_ { member } => {
                let mut members = vec![];
                for m in member {
                    let EnumMember { name, value } = m;
                    members.push(quote!((#name, #value)));
                }
                quote!(usecop::DataInfo::Enum {
                    members: &[#(#members),*],
                })
            }
            DataInfo::Array { minlen, maxlen, members } => quote!(usecop::DataInfo::Array {
                minlen: #minlen, maxlen: #maxlen, members: Box::new(#members)
            }),
            DataInfo::Tuple { member } => {
                let mut members = vec![];
                for m in member {
                    members.push(quote!(#m));
                }
                quote!(usecop::DataInfo::Tuple {
                    members: &[#(#members),*],
                })
            }
            DataInfo::Struct { member } => {
                let mut members = vec![];
                for m in member {
                    let StructMember { name, datainfo } = m;
                    members.push(quote!((#name, #datainfo)));
                }
                quote!(usecop::DataInfo::Struct {
                    members: &[#(#members),*],
                })
            }
            DataInfo::Rust(name) => {
                let ident = format_ident!("{}", name);
                quote!(<#ident as usecop::datamodel::CustomDataInfo>::DATAINFO)
            }
        }.to_tokens(tokens)
    }
}

impl DataInfo {
    fn rust_type(&self) -> TokenStream {
        match self {
            DataInfo::Null {} => quote!(()),
            DataInfo::Bool {} => quote!(bool),
            DataInfo::Double { .. } => quote!(f64),
            DataInfo::Scaled { .. } => quote!(i64),
            DataInfo::Int { .. } => quote!(i64),
            DataInfo::Enum_ { .. } => quote!(i32),
            DataInfo::Tuple { member } => {
                let mut members = vec![];
                for m in member {
                    members.push(m.rust_type());
                }
                quote!((#(#members),*))
            }
            DataInfo::Rust(name) => {
                let ident = format_ident!("{}", name);
                quote!(#ident)
            }
            DataInfo::Str { .. } => panic!("can't represent Str"),
            DataInfo::Blob { .. } => panic!("can't represent Blob"),
            DataInfo::Array { .. } => panic!("can't represent Array"),
            DataInfo::Struct { .. } => panic!("can't represent Struct"),
        }
    }
}

#[derive(FromMeta, Debug)]
struct EnumMember { name: String, value: i32 }

#[derive(FromMeta, Debug)]
struct StructMember { name: String, datainfo: DataInfo }

#[proc_macro_derive(Module, attributes(secop))]
pub fn derive_module(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let input = parse_macro_input!(input as DeriveInput);
    let mut attrs = Module::from_derive_input(&input).unwrap();

    attrs.param.push(Param {
        name: "pollinterval".to_string(),
        doc: "polling interval in seconds".to_string(),
        datainfo: DataInfo::Double {
            min: Some(0.1), max: Some(3600.0), unit: Some("s".to_string()),
            fmtstr: None, abs_res: None, rel_res: None,
        },
        readonly: false,
        group: None,
        polling: Some(false),
        generate_accessors: None,
    });

    let Module { interface, feature, param, command } = &attrs;

    let mut datainfos = vec![];
    let mut accessibles = vec![];
    for param in param {
        let Param { name, doc, readonly, datainfo, group, .. } = param;
        let group = optionize(group);
        let di_name = format_ident!("DATAINFO_P_{}", name.to_uppercase());
        accessibles.push(quote! {
            (#name, usecop::AccessibleDescription {
                description: #doc,
                readonly: #readonly,
                datainfo: &#di_name,
                group: #group,
            })
        });
        datainfos.push(quote! {
            const #di_name: usecop::DataInfo = #datainfo;
        });
    }
    for command in command {
        let Command { name, doc, argument, result, group } = command;
        let group = optionize(group);
        let di_arg_name = format_ident!("DATAINFO_CA_{}", name.to_uppercase());
        let di_res_name = format_ident!("DATAINFO_CR_{}", name.to_uppercase());
        accessibles.push(quote! {
            (#name, usecop::AccessibleDescription {
                description: #doc,
                readonly: false,
                datainfo: &usecop::DataInfo::Command {
                    argument: &#di_arg_name,
                    result: &#di_res_name,
                },
                group: #group,
            })
        });
        datainfos.push(quote! {
            const #di_arg_name: usecop::DataInfo = #argument;
            const #di_res_name: usecop::DataInfo = #result;
        });
    }

    let mut read_impl = vec![];
    let mut change_impl = vec![];
    let mut poll_impl = vec![];
    let mut accessors = vec![];
    for param in param {
        let Param { name, datainfo, readonly, generate_accessors, polling, .. } = param;
        let ident = format_ident!("{}", name);
        let read_method = format_ident!("read_{}", name);
        let write_method = format_ident!("write_{}", name);
        let di_name = format_ident!("DATAINFO_P_{}", name.to_uppercase());

        read_impl.push(quote! {
            Some(#name) => {
                match self.#read_method() {
                    // TODO: check return val?
                    Ok(value) => reply.send(
                        usecop::proto::OutMsg::Update { spec, value, time }),
                    Err(e) => reply.send(e.spec_msg(usecop::wire::READ, spec)),
                }
            }
        });

        let regular_poll = polling != &Some(false) && generate_accessors != &Some(true);
        poll_impl.push(quote! {
            if #regular_poll || all {
                match self.#read_method() {
                    Ok(value) => reply.distribute(usecop::proto::OutMsg::Update {
                        spec: usecop::proto::Specifier::new(name, #name), value, time
                    }),
                    Err(e) => reply.distribute(e.spec_msg(usecop::wire::UPDATE,
                                                          usecop::proto::Specifier::new(name, #name))),
                }
            }
        });

        if *readonly {
            change_impl.push(quote! {
                Some(#name) => {
                    return reply.send(usecop::Error::read_only()
                                      .spec_msg(usecop::wire::CHANGE, spec))
                }
            });
        } else {
            change_impl.push(quote! {
                Some(#name) => {
                    let val = match usecop::FromJson::from_json(value, &#di_name) {
                        Ok(value) => value,
                        Err(e) => return reply.send(e.spec_msg(usecop::wire::CHANGE, spec)),
                    };
                    match self.#write_method(val) {
                        Ok(value) => {
                            // TODO: check return val?
                            reply.distribute(usecop::proto::OutMsg::Update {
                                spec: spec.clone(), value, time });
                            reply.send(usecop::proto::OutMsg::Changed { spec, value, time })
                        }
                        Err(e) => reply.send(e.spec_msg(usecop::wire::CHANGE, spec)),
                    }
                }
            });
        }

        if generate_accessors == &Some(true) {
            let ptype = datainfo.rust_type();
            accessors.push(quote! {
                fn #read_method(&mut self) -> usecop::Result<#ptype> {
                    Ok(self.#ident)
                }
                fn #write_method(&mut self, val: #ptype) -> usecop::Result<#ptype> {
                    self.#ident = val;
                    Ok(val)
                }
            });
        }
    }

    let mut do_impl = vec![];
    for command in command {
        let Command { name, .. } = command;
        let do_method = format_ident!("do_{}", name);
        let di_arg_name = format_ident!("DATAINFO_CA_{}", name.to_uppercase());
        let di_res_name = format_ident!("DATAINFO_CR_{}", name.to_uppercase());
        do_impl.push(quote! {
            Some(#name) => {
                let val = match usecop::FromJson::from_json(arg, &#di_arg_name) {
                    Ok(value) => value,
                    Err(e) => return reply.send(e.spec_msg(usecop::wire::DO, spec)),
                };
                let res = match self.#do_method(val) {
                    Ok(value) => value,
                    Err(e) => return reply.send(e.spec_msg(usecop::wire::DO, spec)),
                };
                match usecop::ToJson::check(&res, &#di_res_name) {
                    Ok(_) => reply.send(usecop::proto::OutMsg::Done { spec, value: res, time }),
                    Err(e) => reply.send(e.spec_msg(usecop::wire::DO, spec)),
                }
            }
        });
    }

    let name = &input.ident;
    quote! {
        impl #name {
            fn read_pollinterval(&mut self) -> usecop::Result<f64> {
                Ok(self.internals.pollinterval)
            }
            fn write_pollinterval(&mut self, val: f64) -> usecop::Result<f64> {
                self.internals.pollinterval = val;
                Ok(self.internals.pollinterval)
            }

            #(#accessors)*
        }

        const _: () = {

            #(#datainfos)*

            impl usecop::Module for #name {
                fn describe(&self) -> usecop::ModuleDescription {
                    usecop::ModuleDescription {
                        description: self.internals.description,
                        implementation: "microSECoP",
                        interface_classes: &[#(#interface),*],
                        features: &[#(#feature),*],
                        accessibles: &[#(#accessibles),*],
                    }
                }

                fn read(&mut self, time: usecop::proto::Timestamp, spec: usecop::proto::Specifier,
                        mut reply: usecop::node::Sender) {
                    match &spec.param {
                        #(#read_impl)*
                        _ => reply.send(usecop::Error::no_param().spec_msg(usecop::wire::READ, spec))
                    }
                }

                fn change(&mut self, time: usecop::proto::Timestamp, spec: usecop::proto::Specifier,
                          value: &mut str, mut reply: usecop::node::Sender) {
                    match &spec.param {
                        #(#change_impl)*
                        _ => reply.send(usecop::Error::no_param().spec_msg(usecop::wire::CHANGE, spec))
                    }
                }

                fn do_(&mut self, time: usecop::proto::Timestamp, spec: usecop::proto::Specifier,
                       arg: &mut str, mut reply: usecop::node::Sender) {
                    match &spec.param {
                        #(#do_impl)*
                        _ => reply.send(usecop::Error::no_command().spec_msg(usecop::wire::DO, spec))
                    }
                }

                fn poll(&mut self, time: usecop::proto::Timestamp, name: &str, all: bool,
                        reply: &mut usecop::node::Sender) {
                    if all || time.any() >= self.internals.lastpoll + self.internals.pollinterval {
                        self.internals.lastpoll = time.any();
                        #(#poll_impl)*
                    }
                }
            }
        };
    }.into()
}


#[proc_macro_derive(Modules)]
pub fn derive_modules(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let input = parse_macro_input!(input as DeriveInput);
    let name = &input.ident;

    let fields = match &input.data {
        Data::Struct(DataStruct { fields: Fields::Named(n), .. }) => &n.named,
        _ => panic!("Modules can only be derived for structs with named fields")
    };

    let count = fields.len();
    let by_name = fields.iter().map(|field| {
        let ident = field.ident.as_ref().unwrap();
        let name = ident.to_string();
        quote! { if name == #name { return Some(&mut self.#ident); } }
    }).collect::<Vec<_>>();
    let for_each = fields.iter().map(|field| {
        let ident = field.ident.as_ref().unwrap();
        let name = ident.to_string();
        quote! { f(#name, &mut self.#ident); }
    }).collect::<Vec<_>>();
    let describe = fields.iter().map(|field| {
        let ident = field.ident.as_ref().unwrap();
        let name = ident.to_string();
        quote! { (#name, usecop::Module::describe(&self.#ident)) }
    }).collect::<Vec<_>>();

    quote! {
        impl usecop::Modules for #name {
            fn count(&self) -> usize {
                #count
            }

            fn by_name(&mut self, name: &str) -> Option<&mut dyn usecop::Module> {
                #(#by_name)*
                None
            }

            fn for_each(&mut self, mut f: impl FnMut(&str, &mut dyn usecop::Module)) {
                #(#for_each)*
            }

            fn describe(&self, eq_id: &str, desc: &str, mut sender: usecop::node::Sender) {
                let msg: usecop::proto::OutMsg<()> = usecop::proto::OutMsg::Describing {
                    id: ".",
                    structure: usecop::NodeDescription {
                        equipment_id: eq_id,
                        description: desc,
                        firmware: "microSECoP",
                        version: "2021.02",
                        interface: "tcp://10767",
                        modules: &[#(#describe),*],
                    }
                };
                sender.send(msg);
            }
        }
    }.into()
}

#[proc_macro_derive(DataInfo, attributes(secop))]
pub fn derive_datainfo(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let input = parse_macro_input!(input as DeriveInput);
    let name = &input.ident;

    match &input.data {
        // Data::Struct(DataStruct { fields: Fields::Named(n), .. }) =>
        //     derive_datainfo_struct(name, n.named.iter()),
        Data::Enum(DataEnum { variants: v, .. }) if is_plain_enum(v.iter()) =>
            derive_datainfo_enum(name, v.iter()),
        _ => panic!("DataInfo can only be derived for structs with named fields \
                     or plain enums")
    }
}

fn is_plain_enum<'a>(variants: impl Iterator<Item=&'a Variant>) -> bool {
    for v in variants {
        if !matches!(v.fields, Fields::Unit) || v.discriminant.is_none() {
            return false;
        }
    }
    true
}

fn derive_datainfo_enum<'a>(name: &Ident, variants: impl Iterator<Item=&'a Variant>) -> proc_macro::TokenStream {
    let mut members = vec![];
    let mut matches = vec![];
    for v in variants {
        let ident = &v.ident;
        let string = v.ident.to_string();
        let value = match &v.discriminant {
            Some((_, Expr::Lit(ExprLit { lit: Lit::Int(i), .. }))) => i.base10_parse::<i32>().unwrap(),
            _ => panic!("Enum variants must have integer discriminants"),
        };
        members.push(quote!((#string, #value)));
        matches.push(quote!(#value => Ok(#name :: #ident),));
    }
    quote! {
        impl usecop::datamodel::CustomDataInfo for #name {
            const DATAINFO: usecop::DataInfo<'static> = usecop::DataInfo::Enum {
                members: &[#(#members),*],
            };
        }

        impl usecop::FromJson<'_> for #name {
            fn from_json<'a>(json: &mut str, _: &usecop::DataInfo<'a>) -> usecop::Result<'a, Self> {
                let val = json.parse::<i32>().map_err(|_| usecop::Error::bad_value("expected int"))?;
                match val {
                    #(#matches)*
                    _ => Err(usecop::Error::bad_value("invalid enum value")),
                }
            }
        }

        impl usecop::ToJson for #name {
            fn to_json(&self, f: &mut core::fmt::Formatter) -> core::fmt::Result {
                write!(f, "{}", *self as i32)
            }
            fn check<'a>(&self, _: &usecop::DataInfo<'a>) -> usecop::Result<'a, ()> {
                Ok(())  // Can't ever be a wrong value.
            }
        }
    }.into()
}
