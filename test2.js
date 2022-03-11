pub struct Account {
    pub lamports: u64,
    pub data: Vec<u8>,
    pub owner: Pubkey,
    pub executable: bool,
  	pub rent_epoch: Epoch,
}
entrypoint!(process_instruction);
pub fn process_instruction(
    program_id: &Pubkey,
    accounts: &[AccountInfo],
    instruction_data: &[u8],
) -> ProgramResult {
    process_somehow(program_id, accounts, instruction_data)
}
pub struct AccountInfo<'a> {
    pub key: &'a Pubkey,
    pub is_signer: bool,
    pub is_writable: bool,
    pub lamports: Rc<RefCell<&'a mut u64>>,
    pub data: Rc<RefCell<&'a mut [u8]>>,
    pub owner: &'a Pubkey,
    pub executable: bool,
    pub rent_epoch: u64,
}
pub fn decompress(&self) -> Option<EdwardsPoint> {
  let Y = FieldElement::from_bytes(self.as_bytes());
  let Z = FieldElement::one();
  let YY = Y.square();
  let u = &YY - &Z;                            // u =  y²-1
  let v = &(&YY * &constants::EDWARDS_D) + &Z; // v = dy²+1
  let (is_valid_y_coord, mut X) = FieldElement::sqrt_ratio(&u, &v);

  if is_valid_y_coord.unwrap_u8() != 1u8 { return None; }

  // FieldElement::sqrt_ratio always returns
  // the nonnegative square root,
  // so we negate according to the supplied sign bit.
  let compressed_sign_bit = Choice::from(self.as_bytes()[31] >> 7);
  X.conditional_negate(compressed_sign_bit);

  Some(EdwardsPoint{ X, Y, Z, T: &X * &Y })
}
pub fn sqrt_ratio(
  u: &FieldElement,
  v: &FieldElement
) -> (u8, FieldElement) {
  // Using the same trick as in ed25519 decoding, we merge the
  // inversion, the square root, and the square test as follows.
  //
  // To compute sqrt(α), we can compute β = α^((p+3)/8).
  // Then β^2 = ±α, so multiplying β by sqrt(-1) if necessary
  // gives sqrt(α).
  //
  // To compute 1/sqrt(α), we observe that
  //    1/β = α^(p-1 - (p+3)/8) = α^((7p-11)/8)
  //                            = α^3 * (α^7)^((p-5)/8).
  //
  // We can therefore compute sqrt(u/v) = sqrt(u)/sqrt(v)
  // by first computing
  //    r = u^((p+3)/8) v^(p-1-(p+3)/8)
  //      = u u^((p-5)/8) v^3 (v^7)^((p-5)/8)
  //      = (uv^3) (uv^7)^((p-5)/8).
  //
  // If v is nonzero and u/v is square, then r^2 = ±u/v,
  //                                     so vr^2 = ±u.
  // If vr^2 =  u, then sqrt(u/v) = r.
  // If vr^2 = -u, then sqrt(u/v) = r*sqrt(-1).
  //
  // If v is zero, r is also zero.

  let v3 = &v.square()  * v;
  let v7 = &v3.square() * v;
  let mut r = &(u * &v3) * &(u * &v7).pow_p58();
  let check = v * &r.square();

  let correct_sign_sqrt = check.ct_eq(   u);
  let flipped_sign_sqrt = check.ct_eq(&(-u));

  let r_prime = &constants::SQRT_M1 * &r;
  r.conditional_assign(&r_prime, flipped_sign_sqrt);

  let was_nonzero_square = correct_sign_sqrt | flipped_sign_sqrt;

  (was_nonzero_square, r)
}
pub fn try_find_program_address(
  seeds: &[&[u8]],
  program_id: &Pubkey
) -> Option<(Pubkey, u8)> {
  let mut bump_seed = [std::u8::MAX];
  for _ in 0..std::u8::MAX {
    let mut seeds_with_bump = seeds.to_vec();
    seeds_with_bump.push(&bump_seed);
    match Self::create_program_address(&seeds_with_bump, program_id) {
      Ok(address) => return Some((address, bump_seed[0])),
      Err(PubkeyError::InvalidSeeds) => (),
      _ => break,
    }
  	bump_seed[0] -= 1;
  }
	None
}
