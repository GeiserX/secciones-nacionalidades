// The two things that differ between the browser and the Android/iOS apps:
// where the user is, and how a file leaves the app.
import { Capacitor } from '@capacitor/core';

export const isNative = Capacitor.isNativePlatform();

/** Current position as { lat, lng }. */
export async function currentPosition() {
  if (isNative) {
    const { Geolocation } = await import('@capacitor/geolocation');
    const perm = await Geolocation.requestPermissions({ permissions: ['location'] });
    if (perm.location === 'denied') throw new Error('permission');
    const p = await Geolocation.getCurrentPosition({ enableHighAccuracy: false, timeout: 15000 });
    return { lat: p.coords.latitude, lng: p.coords.longitude };
  }
  return new Promise((resolve, reject) => {
    if (!navigator.geolocation) return reject(new Error('unsupported'));
    navigator.geolocation.getCurrentPosition(
      (p) => resolve({ lat: p.coords.latitude, lng: p.coords.longitude }),
      (e) => reject(new Error(e.code === 1 ? 'permission' : 'unavailable')),
      { enableHighAccuracy: false, timeout: 15000 },
    );
  });
}

/** Saves a text file: a download in the browser, the share sheet in the apps. */
export async function saveTextFile(filename, text, mime) {
  if (isNative) {
    const { Filesystem, Directory, Encoding } = await import('@capacitor/filesystem');
    const { Share } = await import('@capacitor/share');
    const { uri } = await Filesystem.writeFile({ path: filename, data: text, directory: Directory.Cache, encoding: Encoding.UTF8 });
    await Share.share({ title: filename, files: [uri] });
    return;
  }
  const url = URL.createObjectURL(new Blob([text], { type: mime }));
  const a = Object.assign(document.createElement('a'), { href: url, download: filename });
  document.body.append(a);
  a.click();
  a.remove();
  setTimeout(() => URL.revokeObjectURL(url), 1000);
}
