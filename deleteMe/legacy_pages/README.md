# Legacy Pages - TO BE DELETED

This directory contains the original LiveView pages and layouts that will be deleted after the LiveComponent refactoring is complete.

## Files to be deleted:

### LiveViews
- `arsenal_live.ex` - Original Arsenal command center
- `arsenal_live.html.heex` - Arsenal page template
- `docs_live.ex` - Original documentation center  
- `docs_live.html.heex` - Documentation page template
- `supervisor_live.ex` - Original supervisor monitoring
- `supervisor_live.html.heex` - Supervisor page template
- `system_dashboard_live.ex` - Original system dashboard
- `system_dashboard_live.html.heex` - Dashboard page template

### Layouts
- `layouts.ex` - Original layout component
- `app.html.heex` - Application layout template
- `root.html.heex` - Root HTML template

## Replacement Strategy

These files are being replaced with:
1. New LiveComponent-based pages in `lib/otp_supervisor_web/live/`
2. Terminal-themed components in `lib/otp_supervisor_web/components/terminal/`
3. Reusable widgets in `lib/otp_supervisor_web/components/widgets/`
4. Flexible layouts in `lib/otp_supervisor_web/components/layout/`

## Timeline

- **Phase 1**: Create foundation LiveComponents
- **Phase 2**: Build new pages using LiveComponents  
- **Phase 3**: Update router to point to new pages
- **Phase 4**: Delete this entire directory

---

**DO NOT MODIFY THESE FILES** - They are kept only for reference during the refactoring process.