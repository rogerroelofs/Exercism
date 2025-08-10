module PhoneNumber
  def self.clean(phone)
    phone = phone.gsub(/\D/, '')
    return nil unless phone.match?(/\A1?[2-9]\d{2}[2-9]\d{6}\z/)
    phone.length == 11 ? phone[1..-1] : phone
  end
end